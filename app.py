import streamlit as st
import streamlit.components.v1 as components
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
from matplotlib.lines import Line2D
import itertools
import re
from io import BytesIO
from matplotlib import font_manager

st.markdown(" 본 시각화 툴은 고려대학교 경제학과 진리장학 학부연구과정의 결과물임을 명시합니다.  \n지도교수 : 한 치 록 · 학부연구생 : 정 보 현")

st.title("회귀모형 시각화 툴")

st.markdown("### 📋 회귀 결과 추출용 R 코드")
st.caption(" 복사한 코드를 R에서 실행하면 시각화에 적합한 데이터 파일이 생성됩니다.")
r_code = """if (!require("broom"))  install.packages("broom")
if (!require("dplyr"))  install.packages("dplyr")
if (!require("readr"))  install.packages("readr")
library(broom); library(dplyr); library(readr)

if (!exists("model")) stop("먼저 model <- lm(...) 등으로 회귀모형을 설정하세요.")

dep_var <- names(model.frame(model))[1]

df_used <- as.data.frame(model.frame(model))

glance_tbl <- glance(model) %>%
  dplyr::transmute(r_squared = r.squared)

ct <- summary(model)$coefficients

coef_tbl <- data.frame(
  term      = rownames(ct),
  estimate  = ct[, "Estimate"],
  std.error = ct[, "Std. Error"],
  p.value   = ct[, "Pr(>|t|)"],
  dep_var   = dep_var,
  row.names = NULL,
  check.names = FALSE
)

ci <- confint(model, level = 0.95)
ci_tbl <- data.frame(
  term      = rownames(ci),
  conf.low  = ci[, 1],
  conf.high = ci[, 2],
  row.names = NULL,
  check.names = FALSE
)

tidy_model <- dplyr::left_join(coef_tbl, ci_tbl, by = "term")

num_cols <- names(df_used)[sapply(df_used, is.numeric)]
feature_cols <- setdiff(num_cols, dep_var)

if (length(feature_cols) == 0) {
  q_tbl <- data.frame(term=character(0), q1=double(0), q7=double(0))
} else {
  q_list <- lapply(feature_cols, function(v) {
    x <- df_used[[v]]
    x <- x[is.finite(x)]
    if (length(x) == 0) return(data.frame(term=v, q1=NA_real_, q7=NA_real_))
    data.frame(
      term = v,
      q1 = as.numeric(stats::quantile(x, 0.125, na.rm = TRUE)),
      q7 = as.numeric(stats::quantile(x, 0.875, na.rm = TRUE))
    )
  })
  q_tbl <- dplyr::bind_rows(q_list)
}

export_tbl <- dplyr::left_join(tidy_model, q_tbl, by = "term")
export_tbl$r_squared <- glance_tbl$r_squared

readr::write_csv(export_tbl, "regression_result.csv")
"""

html_code = f"""
<div>
  <textarea id=\"code\" style=\"display:none;\">{r_code}</textarea>
  <button onclick=\"navigator.clipboard.writeText(document.getElementById('code').value)\"
          style=\"padding:8px 16px; font-size:14px; background-color:#4CAF50; color:white; border:none; border-radius:6px; cursor:pointer;\">
    📄 R 코드 복사하기
  </button>
</div>
"""
components.html(html_code, height=60)

st.markdown("""
#### 📌 데이터 추출 전 반드시 확인하세요
- 위에 제공된 코드는 **R에서 회귀분석을 이미 수행한 상태**에서만 정상 작동합니다.  
- 이때 **회귀분석 코드로 반드시 `model <- lm()`을 사용해야만 합니다.**
- 만일 **`model <- lm()`이 아닌 `df <- lm()`의 형태로 회귀분석을 한 뒤 위의 데이터 추출 코드를 실행하면 데이터가 추출되지 않습니다.**
- 코드 실행 후 생성된 `regression_result.csv` 파일은 기본적으로 R의 **작업 디렉토리**에 저장됩니다.
- 저장 경로를 직접 확인하려면 R에서 다음 명령을 실행해보세요. **getwd()**
""")

uploaded_file = st.file_uploader("📁 회귀 결과 CSV 파일 업로드", type=["csv"])

if uploaded_file is not None:
    df = pd.read_csv(uploaded_file)
    dep_var = df["dep_var"].iloc[0]
    intercept_row = df[df["term"].str.lower().str.contains("intercept")]
    if not intercept_row.empty:
        intercept = intercept_row["estimate"].values[0]
    else:
        intercept = df["estimate"].iloc[0] 
    coeffs_all = dict(zip(df["term"], df["estimate"]))
    coeffs = {k: v for k, v in coeffs_all.items() if "intercept" not in k.lower()}
    all_terms = [t for t in df["term"] if "intercept" not in t.lower()]

    if "r_squared" in df.columns:
        r_squared = df["r_squared"].iloc[0]
    else:
        r_squared = None

    model_type = st.selectbox(
        "회귀모형 유형 선택",
        ["선형모형", "제곱항 모형", "상호작용항 모형", "제곱항 + 상호작용항 모형"]
    )
    base_vars = st.multiselect("시각화 변수 선택: 평면(가로축 변수) / 입체(가로·세로축 변수)", all_terms)

    if len(base_vars) > 2:
        st.error("시각화는 최대 2개의 설명변수까지 지원됩니다.")
        st.stop()

    squared_vars, interaction_vars = [], []
    squared_mapping, interaction_mapping = {}, {}
    dummy_vars = []
    dummy_values = [0, 1]

    if model_type in ["제곱항 모형", "제곱항 + 상호작용항 모형"]:
        squared_vars = st.multiselect("제곱항 선택 (예: age^2)", [t for t in all_terms if t not in base_vars])
        squared_mapping = {}
        for sq_var in squared_vars:
            base = st.selectbox(f"➡️ '{sq_var}'은(는) 어떤 변수의 제곱인가요?", [t for t in base_vars if t not in squared_vars], key=f"base_{sq_var}")
            squared_mapping[sq_var] = base
            
    if model_type in ["선형모형", "제곱항 모형", "상호작용항 모형", "제곱항 + 상호작용항 모형"]:
        st.markdown("#### ⚙️ 더미변수 설정")

        use_dummy = st.checkbox("더미변수가 포함되어 있나요?", value=False)

        if use_dummy:
            dummy_vars = st.multiselect(
                "더미변수를 선택하세요 (1개만 선택 가능)",
                options=[t for t in all_terms],
                help="0/1 값을 가지는 변수만 선택하세요. (예: gender, smoker 등)"
            )
            if len(dummy_vars) == 0:
                st.warning("⚠️ 더미변수를 선택해주세요.")
            elif len(dummy_vars) > 1:
                st.error("❌ 더미변수는 1개만 지원됩니다.")
                st.stop()
        else:
            dummy_vars = []
            
        if model_type in ["상호작용항 모형", "제곱항 + 상호작용항 모형"]:
            interaction_candidates = base_vars + dummy_vars

            interaction_vars = st.multiselect("상호작용항 선택", [t for t in all_terms if t not in base_vars])

            interaction_mapping = {}
            for inter_var in interaction_vars:
                col1, col2 = st.columns(2)
                with col1:
                    var1 = st.selectbox(f"➡️ '{inter_var}'의 첫 번째 항", [t for t in interaction_candidates if t != inter_var], key=f"inter1_{inter_var}")
                with col2:
                    var2 = st.selectbox(f"➡️ '{inter_var}'의 두 번째 항", [t for t in interaction_candidates if t != inter_var and t != var1], key=f"inter2_{inter_var}")
                interaction_mapping[inter_var] = (var1, var2)

    st.markdown("### ✅ 회귀분석요약")

    coef = df[["term","estimate","std.error","p.value","conf.low","conf.high"]].copy()

    for c in ["estimate","std.error","conf.low","conf.high"]:
        coef[c] = coef[c].round(4)

    def fmt_p(p):
        if pd.isna(p): return ""
        if p < 0.0001: return "<0.0001"
        return f"{p:.4f}"
    coef["p.value"] = coef["p.value"].apply(fmt_p)

    with st.expander("회귀 요약 (추정값 / 표준오차 / p값 / 95% 신뢰구간)", expanded=False):
        st.dataframe(coef, use_container_width=True, hide_index=True)
        st.caption(r"귀무가설 $H_0: \beta_j = 0$")

    st.markdown("### 🧾 회귀식")
    terms_list = [f"{coeffs.get(v, 0):+.3f}·{v}" for v in base_vars]
    if dummy_vars:
        for dummy in dummy_vars:
            if dummy not in base_vars:
                terms_list.append(f"{coeffs.get(dummy, 0):+.3f}·{dummy}")
    for v, base in squared_mapping.items():
        terms_list.append(f"{coeffs.get(v, 0):+.3f}·{base}²")
    for v, (var1, var2) in interaction_mapping.items():
        terms_list.append(f"{coeffs.get(v, 0):+.3f}·{var1}·{var2}")
    eq = f"{dep_var} = {intercept:.3f} " + " ".join(terms_list)
    st.latex(eq.replace("·", r"\cdot ").replace("²", "^{2}"))

    st.markdown("---")
    st.markdown("### 회귀모형 요약")
    st.markdown(f"- **모형유형:** {model_type}")
    st.markdown(f"- **종속변수:** {dep_var}")
    st.markdown(f"- **설명변수:** {', '.join(base_vars)}")

    def get_initial_range(var, df, default=(0.0, 10.0)):
        try:
            if {"term", "q1", "q7"}.issubset(df.columns):
                mask = df["term"].astype(str).str.strip().eq(str(var))
                if mask.any():
                    q1 = pd.to_numeric(df.loc[mask, "q1"], errors="coerce").dropna()
                    q7 = pd.to_numeric(df.loc[mask, "q7"], errors="coerce").dropna()
                    if not q1.empty and not q7.empty:
                        q1v, q7v = float(q1.iloc[0]), float(q7.iloc[0])
                        if np.isfinite(q1v) and np.isfinite(q7v) and (q7v > q1v):
                            return q1v, q7v
        except Exception:
            pass
        return default

    if "init_ranges" not in st.session_state:
        st.session_state["init_ranges"] = {}
    for v in base_vars:
        if v not in st.session_state["init_ranges"]:
            st.session_state["init_ranges"][v] = get_initial_range(v, df, default=(0.0, 10.0))
        mn, mx = st.session_state["init_ranges"][v]
        if f"min_{v}" not in st.session_state:
            st.session_state[f"min_{v}"] = float(mn)
        if f"max_{v}" not in st.session_state:
            st.session_state[f"max_{v}"] = float(mx)

    if st.button("↩︎ 범위 모두 원상복귀"):
        for v in base_vars:
            mn, mx = st.session_state["init_ranges"][v]
            st.session_state[f"min_{v}"] = float(mn)
            st.session_state[f"max_{v}"] = float(mx)
        st.rerun()

    ranges = {}
    for var in base_vars:
        col1, col2 = st.columns(2)
        with col1:
            min_val = st.number_input(f"{var} 최소값", key=f"min_{var}")
        with col2:
            max_val = st.number_input(f"{var} 최대값", key=f"max_{var}")
        ranges[var] = (min_val, max_val)

    all_vars = base_vars + dummy_vars
    var_ranges = [
        np.linspace(*ranges[var], 200) if var in base_vars else [0, 1]
        for var in all_vars
    ]
    combinations = list(itertools.product(*var_ranges))

    z_values = []
    for combo in combinations:
        inputs = dict(zip(all_vars, combo))  
        z = intercept

        for var in all_vars:   
            z += coeffs.get(var, 0) * inputs.get(var,0)

        for var, base in squared_mapping.items():
            z += coeffs.get(var, 0) * (inputs.get(base, 0) ** 2)

        for var, (var1, var2) in interaction_mapping.items():
            z += coeffs.get(var, 0) * inputs.get(var1, 0) * inputs.get(var2, 0)

        z_values.append(z)

    z_min_auto = min(z_values)
    z_max_auto = max(z_values)

    col_z1, col_z2 = st.columns(2)
    with col_z1:
        z_min_user = st.number_input("종속변수 최소값", value=z_min_auto)
    with col_z2:
        z_max_user = st.number_input("종속변수 최대값", value=z_max_auto)

    st.caption(f"종속변수 범위(설명변수 기준) : {z_min_auto:.2f} ~ {z_max_auto:.2f}")

    def fig_to_bytes(fig, fmt="png", dpi=200, transparent=False):
        buf = BytesIO()
        fig.savefig(buf, format=fmt, dpi=dpi, bbox_inches="tight", transparent=transparent)
        buf.seek(0)
        return buf

    try:
        font_path = font_manager.findSystemFonts(fontpaths=None, fontext='ttf')
        font_candidates = [f for f in font_path if "NotoSansCJK" in f or "Noto Sans CJK" in f]
        if font_candidates:
            matplotlib.rcParams['font.family'] = font_manager.FontProperties(fname=font_candidates[0]).get_name()
        else:
            matplotlib.rcParams['font.family'] = 'Malgun Gothic'
    except:
        matplotlib.rcParams['font.family'] = 'Malgun Gothic'

    matplotlib.rcParams['axes.unicode_minus'] = False

    if model_type == "선형모형":
        if len(base_vars) == 1 and len(dummy_vars) == 0:
            var = base_vars[0]
            X = np.linspace(*ranges[var], 100)
            Y = intercept + coeffs.get(var,0) * X

            fig, ax = plt.subplots()
            ax.plot(X, Y)        
            ax.set_xlabel(var) 
            ax.set_ylabel(dep_var)
            ax.set_title(f"Regression result")

            z_min_auto = min(Y)
            z_max_auto = max(Y)
            z_span = z_max_auto - z_min_auto
            z_margin = min(0.05 * z_span, 1)

            if z_max_user > z_min_user:
                ax.set_ylim(z_min_user - z_margin, z_max_user + z_margin)
            else:
                ax.set_ylim(z_min_auto - z_margin, z_max_auto + z_margin)

            if r_squared is not None:
                info_text = f"{var}: {coeffs.get(var,0):.3f}\nR²: {r_squared:.3f}"
            else:
                info_text = f"{var}: {coeffs.get(var,0):.3f}"

            ax.text(0.05, 0.95, info_text, transform=ax.transAxes,
                    fontsize=10, ha='left', va='top',
                    bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))
            st.pyplot(fig)
            st.download_button(
                "PNG 다운로드",
                data=fig_to_bytes(fig, fmt="png", dpi=300),
                file_name=f"{dep_var}_plot.png",
                mime="image/png"
            )

        if len(base_vars) == 1 and len(dummy_vars) == 1:
            var = base_vars[0]
            dummy_var = dummy_vars[0]

            X = np.linspace(*ranges[var], 100)
            Y0 = intercept + coeffs.get(var,0) * X
            Y1 = (
                intercept 
                + coeffs.get(dummy_var, 0)
                + coeffs.get(var,0) * X
            )

            fig, ax = plt.subplots()
            ax.plot(X, Y0, label=f"{dummy_var}=0", color="blue")
            ax.plot(X, Y1, label=f"{dummy_var}=1", color="orange")

            ax.set_xlabel(var)
            ax.set_ylabel(dep_var)
            ax.set_title("Regression result")
            ax.legend()

            z_min_auto = min(np.min(Y0), np.min(Y1))
            z_max_auto = max(np.max(Y0), np.max(Y1))
            z_span = z_max_auto - z_min_auto
            z_margin = min(0.05 * z_span, 1)

            if z_max_user > z_min_user:
                ax.set_ylim(z_min_user - z_margin, z_max_user + z_margin)
            else:
                ax.set_ylim(z_min_auto - z_margin, z_max_auto + z_margin)

            if r_squared is not None:
                info_text = f"{var}: {coeffs.get(var,0):.3f}\nR²: {r_squared:.3f}"
            else:
                info_text = f"{var}: {coeffs.get(var,0):.3f}"

            ax.text(0.05, 0.95, info_text, transform=ax.transAxes,
                    fontsize=10, ha='left', va='top',
                    bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))

            st.pyplot(fig)
            st.download_button(
                "PNG 다운로드",
                data=fig_to_bytes(fig, fmt="png", dpi=300),
                file_name=f"{dep_var}_plot.png",
                mime="image/png"
            )

        elif len(base_vars) == 2 and len(dummy_vars) == 0:
            var1, var2 = base_vars[0], base_vars[1]
            X1 = np.linspace(*ranges[var1], 50)
            X2 = np.linspace(*ranges[var2], 50)
            X1_grid, X2_grid = np.meshgrid(X1, X2)

            Y = (intercept
                 + coeffs.get(var1, 0) * X1_grid
                 + coeffs.get(var2, 0) * X2_grid)

            fig = plt.figure(constrained_layout=True)
            ax = fig.add_subplot(111, projection='3d')
            ax.plot_surface(X1_grid, X2_grid, Y, alpha=0.8, cmap="viridis")
            ax.set_xlabel(var1)
            ax.set_ylabel(var2)
            ax.text2D(
                1.02, 0.84,
                dep_var,
                transform=ax.transAxes,
                fontsize=10,
                rotation=0,
                ha='right', va='top',
                color='black'
            )

            ax.set_title(f"Regression result", fontsize=12)

            if z_max_user > z_min_user:
                ax.set_zlim(z_min_user, z_max_user)
            else:
                ax.set_zlim(z_min_auto, z_max_auto)

            if r_squared is not None:
                info_text = '\n'.join([f'{v} : {coeffs.get(v,0):.3f}' for v in base_vars])\
                            + f"\nR² : {r_squared:.3f}"
            else:
                info_text = '\n'.join([f'{v} : {coeffs.get(v,0):.3f}' for v in base_vars])

            ax.text2D(0.05, 0.95, info_text, transform=ax.transAxes,
                      fontsize=10, ha='left', va='top',
                      bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))

            st.pyplot(fig)
            st.download_button(
                "PNG 다운로드",
                data=fig_to_bytes(fig, fmt="png", dpi=300),
                file_name=f"{dep_var}_plot.png",
                mime="image/png"
            )

        elif len(base_vars) == 2 and len(dummy_vars) == 1:
            dummy_var = dummy_vars[0]
            var1, var2 = base_vars[0], base_vars[1]
            X1 = np.linspace(*ranges[var1], 50)
            X2 = np.linspace(*ranges[var2], 50)
            X1_grid, X2_grid = np.meshgrid(X1, X2)

            Z0 = (intercept
                 + coeffs.get(var1, 0) * X1_grid
                 + coeffs.get(var2, 0) * X2_grid)

            Z1 = (intercept
                 + coeffs.get(dummy_var, 0)
                 + coeffs.get(var1, 0) * X1_grid
                 + coeffs.get(var2, 0) * X2_grid)

            fig = plt.figure(constrained_layout=True)
            ax = fig.add_subplot(111, projection='3d')

            ax.plot_surface(X1_grid, X2_grid, Z0, cmap='Blues', alpha=0.6, label=f'{dummy_var}=0')
            ax.plot_surface(X1_grid, X2_grid, Z1, cmap='Oranges', alpha=0.6, label=f'{dummy_var}=1')

            legend_elements = [
                Line2D([0], [0], marker='o', color='w', label=f"{dummy_var}=0",
                    markerfacecolor='blue', markersize=7),
                Line2D([0], [0], marker='o', color='w', label=f"{dummy_var}=1",
                    markerfacecolor='orange', markersize=7),
            ]

            ax.set_xlabel(var1)
            ax.set_ylabel(var2)
            ax.text2D(
                1.02, 0.84,
                dep_var,
                transform=ax.transAxes,
                fontsize=10,
                rotation=0,
                ha='right', va='top',
                color='black'
            )
            if r_squared is not None:
                info_text = '\n'.join([f'{v} : {coeffs.get(v,0):.3f}' for v in base_vars])\
                            + f"\nR² : {r_squared:.3f}"
            else:
                info_text = '\n'.join([f'{v} : {coeffs.get(v,0):.3f}' for v in base_vars])

            ax.text2D(0.05, 0.95, info_text, transform=ax.transAxes,
                      fontsize=10, ha='left', va='top',
                      bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))

            ax.set_title("Regression result")
            ax.legend(handles=legend_elements, fontsize=9, handletextpad=0.1)
            st.pyplot(fig)
            st.download_button(
                "PNG 다운로드",
                data=fig_to_bytes(fig, fmt="png", dpi=300),
                file_name=f"{dep_var}_plot.png",
                mime="image/png"
            )

    elif model_type == "제곱항 모형":
        if len(base_vars) == 1 and len(dummy_vars) == 0:
            var = base_vars[0]
            X = np.linspace(*ranges[var], 100)
            Y = intercept + coeffs.get(var, 0) * X

            for var_sq, base in squared_mapping.items():
                Y += coeffs.get(var_sq, 0) * (X ** 2)
            
            a = next((coeffs[sq_var] for sq_var, base in squared_mapping.items() if base == var), 0)
            b = coeffs.get(var, 0)
            c = intercept

            x_star = None
            y_star = None
            extremum_type = None
            if a != 0:
                x_star = -b / (2 * a)
                y_star = a * x_star**2 + b * x_star + c
                extremum_type = "최소" if a > 0 else "최대"

            fig, ax = plt.subplots()
            ax.plot(X, Y)
            ax.set_xlabel(var)
            ax.set_ylabel(dep_var)
            if r_squared is not None:
                info_text = f"R²: {r_squared:.3f}"
            else:
                info_text = ""

            ax.text(0.05, 0.95, info_text, transform=ax.transAxes,
                    fontsize=10, ha='left', va='top',
                    bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))

            ax.set_title("Regression result")

            z_min_auto = min(Y)
            z_max_auto = max(Y)
            z_span = z_max_auto - z_min_auto
            z_margin = min(0.05 * z_span, 1)

            if z_max_user > z_min_user:
                ax.set_ylim(z_min_user - z_margin, z_max_user + z_margin)
            else:
                ax.set_ylim(z_min_auto - z_margin, z_max_auto + z_margin)
            
            st.pyplot(fig)
            st.download_button(
                "PNG 다운로드",
                data=fig_to_bytes(fig, fmt="png", dpi=300),
                file_name=f"{dep_var}_plot.png",
                mime="image/png"
            )

            if x_star is not None and y_star is not None:
                with st.expander("반환점(Turning point) 보기", expanded=False):
                    st.markdown(
                        f"- **{var} ≈ {x_star:.2f}**에서 **{dep_var} ≈ {y_star:.2f}**으로 **{extremum_type}**"
                    )
                    range_min, range_max = ranges[var]
                    in_range = (range_min <= x_star <= range_max)
                    if in_range:
                        st.markdown(
                            f"- 현재 설정한 {var} 범위: [{range_min:.2f}, {range_max:.2f}] 안에 반환점 존재"
                        )
                    else:
                        st.markdown(
                            f"- 현재 설정한 {var} 범위: [{range_min:.2f}, {range_max:.2f}] 밖에 반환점 존재"
                        )

                    st.caption("팁: 반환점 근처로 x 범위를 좁히면 곡률이 더 선명하게 보일 수 있어요")

        elif len(base_vars) == 1 and len(dummy_vars) == 1:
            var = base_vars[0]
            dummy_var = dummy_vars[0]
            X = np.linspace(*ranges[var], 100)
            Y0 = intercept + coeffs.get(var, 0) * X
            Y1 = intercept + coeffs.get(dummy_var, 0) + coeffs.get(var, 0) * X

            for var_sq, base in squared_mapping.items():
                Y0 += coeffs.get(var_sq, 0) * (X ** 2)
                Y1 += coeffs.get(var_sq, 0) * (X ** 2)
            
            a = next((coeffs[sq_var] for sq_var, base in squared_mapping.items() if base == var), 0)
            b = coeffs.get(var, 0)
            c0 = intercept
            c1 = intercept + coeffs.get(dummy_var, 0)

            x_star = None
            y0_star = None
            y1_star = None
            extremum_type = None
            if a != 0:
                x_star = -b / (2 * a)
                y0_star = a * x_star**2 + b * x_star + c0
                y1_star = a * x_star**2 + b * x_star + c1
                extremum_type = "최소" if a > 0 else "최대"

            fig, ax = plt.subplots()
            ax.plot(X, Y0, label=f"{dummy_var}=0", color="blue")
            ax.plot(X, Y1, label=f"{dummy_var}=1", color="orange")
            ax.set_xlabel(var)
            ax.set_ylabel(dep_var)
            ax.legend()
            if r_squared is not None:
                info_text = f"R²: {r_squared:.3f}"
            else:
                info_text = ""
            ax.text(0.05, 0.95, info_text, transform=ax.transAxes,
                    fontsize=10, ha='left', va='top',
                    bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))

            ax.set_title("Regression result")

            z_min_auto = min(np.min(Y0), np.min(Y1))
            z_max_auto = max(np.max(Y0), np.max(Y1))
            z_span = z_max_auto - z_min_auto
            z_margin = min(0.05 * z_span, 1)

            if z_max_user > z_min_user:
                ax.set_ylim(z_min_user - z_margin, z_max_user + z_margin)
            else:
                ax.set_ylim(z_min_auto - z_margin, z_max_auto + z_margin)

            st.pyplot(fig)
            st.download_button(
                "PNG 다운로드",
                data=fig_to_bytes(fig, fmt="png", dpi=300),
                file_name=f"{dep_var}_plot.png",
                mime="image/png"
            )

            if x_star is not None and y0_star is not None and y1_star is not None:
                with st.expander("반환점(Turning point) 보기", expanded=False):
                    st.markdown(
                        f"- {dummy_var}=0일 경우 **{var} ≈ {x_star:.2f}**에서 **{dep_var} ≈ {y0_star:.2f}**으로 **{extremum_type}**"
                    )
                    st.markdown(
                        f"- {dummy_var}=1일 경우 **{var} ≈ {x_star:.2f}**에서 **{dep_var} ≈ {y1_star:.2f}**으로 **{extremum_type}**"
                    )
                    range_min, range_max = ranges[var]
                    in_range = (range_min <= x_star <= range_max)
                    if in_range:
                        st.markdown(
                            f"- 현재 설정한 {var} 범위: [{range_min:.2f}, {range_max:.2f}] 안에 반환점 존재"
                        )
                    else:
                        st.markdown(
                            f"- 현재 설정한 {var} 범위: [{range_min:.2f}, {range_max:.2f}] 밖에 반환점 존재"
                        )

                    st.caption("팁: 반환점 근처로 x 범위를 좁히면 곡률이 더 선명하게 보일 수 있어요")

        elif len(base_vars) == 2 and len(dummy_vars) == 0:
            var1, var2 = base_vars[0], base_vars[1]
            X1 = np.linspace(*ranges[var1], 50)
            X2 = np.linspace(*ranges[var2], 50)
            X1_grid, X2_grid = np.meshgrid(X1, X2)
            
            Y = np.full_like(X1_grid, intercept)

            Y += coeffs.get(var1, 0) * X1_grid
            Y += coeffs.get(var2, 0) * X2_grid
            
            a1 = next((coeffs[sq_var] for sq_var, base in squared_mapping.items() if base == var1), 0)
            a2 = next((coeffs[sq_var] for sq_var, base in squared_mapping.items() if base == var2), 0)

            for sq_var, base in squared_mapping.items():
                if base == var1:
                    Y += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                elif base == var2:
                    Y += coeffs.get(sq_var, 0) * (X2_grid ** 2)

            x1_star = None
            x2_star = None
            y_star = None
            extremum_type = None
            b1 = coeffs.get(var1, 0)
            b2 = coeffs.get(var2, 0)
            c = intercept
            x1_star = -b1 / (2 * a1) if a1 != 0 else None
            x2_star = -b2 / (2 * a2) if a2 != 0 else None
            if x1_star is not None and x2_star is not None:
                y_star = a1 * x1_star**2 + a2 * x2_star**2 + b1 * x1_star + b2 * x2_star + c
                range1_min, range1_max = ranges[var1]
                range2_min, range2_max = ranges[var2]
                in_range = (range1_min <= x1_star <= range1_max) and (range2_min <= x2_star <= range2_max)
                if a1 > 0 and a2 > 0:
                    extremum_type = "최소"
                elif a1 < 0 and a2 < 0:
                    extremum_type = "최대"
                else:
                    extremum_type = "극값"

            fig = plt.figure(constrained_layout=True)
            ax = fig.add_subplot(111, projection='3d')
            ax.plot_surface(X1_grid, X2_grid, Y, alpha=0.8, cmap="viridis")
            ax.set_xlabel(var1)
            ax.set_ylabel(var2)
            ax.text2D(
                1.02, 0.84,
                dep_var,
                transform=ax.transAxes,
                fontsize=10,
                rotation=0,
                ha='right', va='top',
                color='black'
            )

            if r_squared is not None:
                info_text = f"R²: {r_squared:.3f}"
            else:
                info_text = ""
            ax.text2D(0.05, 0.95, info_text, transform=ax.transAxes,
                      fontsize=10, ha='left', va='top',
                      bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))

            ax.set_title("Regression result")

            if z_max_user > z_min_user:
                ax.set_zlim(z_min_user, z_max_user)
            else:
                ax.set_zlim(z_min_auto, z_max_auto)

            st.pyplot(fig)
            st.download_button(
                "PNG 다운로드",
                data=fig_to_bytes(fig, fmt="png", dpi=300),
                file_name=f"{dep_var}_plot.png",
                mime="image/png"
            )

            is_extremum = extremum_type in ["최소", "최대"]

            with st.expander("반환점 요약 (Turning point)", expanded=False):
                range1_min, range1_max = ranges[var1]
                range2_min, range2_max = ranges[var2]
                if (x1_star is None) or (x2_star is None) or (y_star is None):
                    st.markdown("- 반환점 좌표를 계산할 수 없습니다.")
                elif not is_extremum:
                    st.markdown("- 이 모형에서는 **최대·최소의 반환점이 존재하지 않습니다.**")
                else:
                    in_range = (range1_min <= x1_star <= range1_max) and (range2_min <= x2_star <= range2_max)
                    st.markdown(
                        f"- 반환점: **{var1} ≈ {x1_star:.2f}**, "
                        f"**{var2} ≈ {x2_star:.2f}**에서 "
                        f"**{dep_var} ≈ {y_star:.2f}**으로**{extremum_type}**"
                    )
                    st.markdown(
                        "- 현재 설정한 설명변수 **범위 안에 반환점이 존재**"
                        if in_range else
                        "- 현재 설정한 설명변수 **범위 밖에 반환점이 존재**"
                    )
                    st.caption("팁: 반환점 근처로 x 범위를 좁히면 곡률이 더 선명하게 보일 수 있어요")

        elif len(base_vars) == 2 and len(dummy_vars) == 1:
            var1, var2 = base_vars[0], base_vars[1]
            dummy_var = dummy_vars[0]
            X1 = np.linspace(*ranges[var1], 50)
            X2 = np.linspace(*ranges[var2], 50)
            X1_grid, X2_grid = np.meshgrid(X1, X2)
            
            Y0 = np.full_like(X1_grid, intercept)
            Y1 = np.full_like(X1_grid, intercept) + coeffs.get(dummy_var,0)
                
            Y0 += coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid
            Y1 += coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid

            for sq_var, base in squared_mapping.items():
                if base == var1:
                    Y0 += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                    Y1 += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                elif base == var2:
                    Y0 += coeffs.get(sq_var, 0) * (X2_grid ** 2)
                    Y1 += coeffs.get(sq_var, 0) * (X2_grid ** 2)

            x1_star = None
            x2_star = None
            y0_star = None
            y1_star = None
            extremum_type = None
            a1 = next((coeffs[sq_var] for sq_var, base in squared_mapping.items() if base == var1), 0)
            a2 = next((coeffs[sq_var] for sq_var, base in squared_mapping.items() if base == var2), 0)
            b1 = coeffs.get(var1, 0)
            b2 = coeffs.get(var2, 0)
            c0 = intercept
            c1 = intercept + coeffs.get(dummy_var,0)
            x1_star = -b1 / (2 * a1) if a1 != 0 else None
            x2_star = -b2 / (2 * a2) if a2 != 0 else None
            if x1_star is not None and x2_star is not None:
                y0_star = a1 * x1_star**2 + a2 * x2_star**2 + b1 * x1_star + b2 * x2_star + c0
                y1_star = a1 * x1_star**2 + a2 * x2_star**2 + b1 * x1_star + b2 * x2_star + c1
                
                range1_min, range1_max = ranges[var1]
                range2_min, range2_max = ranges[var2]
                in_range = (range1_min <= x1_star <= range1_max) and (range2_min <= x2_star <= range2_max)
                if a1 > 0 and a2 > 0:
                    extremum_type = "최소"
                elif a1 < 0 and a2 < 0:
                    extremum_type = "최대"
                else:
                    extremum_type = "극값"

            fig = plt.figure(constrained_layout=True)
            ax = fig.add_subplot(111, projection='3d')
            ax.plot_surface(X1_grid, X2_grid, Y0, alpha=0.8, cmap="Blues", label=f'{dummy_var}=0')
            ax.plot_surface(X1_grid, X2_grid, Y1, alpha=0.8, cmap="Oranges", label=f'{dummy_var}=1')

            legend_elements = [
                Line2D([0], [0], marker='o', color='w', label=f'{dummy_var}=0',
                    markerfacecolor='blue', markersize=7),
                Line2D([0], [0], marker='o', color='w', label=f'{dummy_var}=1',
                    markerfacecolor='orange', markersize=7),
            ]

            ax.set_xlabel(var1)
            ax.set_ylabel(var2)
            ax.legend(handles=legend_elements, fontsize=9, handletextpad=0.1)
            ax.text2D(
                1.02, 0.84,
                dep_var,
                transform=ax.transAxes,
                fontsize=10,
                rotation=0,
                ha='right', va='top',
                color='black'
            )

            if r_squared is not None:
                info_text = f"R²: {r_squared:.3f}"
            else:
                info_text = ""
            ax.text2D(0.05, 0.95, info_text, transform=ax.transAxes,
                      fontsize=10, ha='left', va='top',
                      bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))

            ax.set_title("Regression result")

            if z_max_user > z_min_user:
                ax.set_zlim(z_min_user, z_max_user)
            else:
                ax.set_zlim(z_min_auto, z_max_auto)

            st.pyplot(fig)
            st.download_button(
                "PNG 다운로드",
                data=fig_to_bytes(fig, fmt="png", dpi=300),
                file_name=f"{dep_var}_plot.png",
                mime="image/png"
            )

            is_extremum = extremum_type in ["최소", "최대"]

            with st.expander("반환점 요약 (Turning point)", expanded=False):
                range1_min, range1_max = ranges[var1]
                range2_min, range2_max = ranges[var2]
                if (x1_star is None) or (x2_star is None) or (y0_star is None) or (y1_star is None):
                    st.markdown("- 반환점 좌표를 계산할 수 없습니다.")
                elif not is_extremum:
                    st.markdown("- 이 모형에서는 **최대·최소의 반환점이 존재하지 않습니다.**")
                else:
                    in_range = (range1_min <= x1_star <= range1_max) and (range2_min <= x2_star <= range2_max)
                    st.markdown(
                        f"- {dummy_var}=0일 경우 **{var1} ≈ {x1_star:.2f}, {var2} ≈ {x2_star:.2f}**에서 **{dep_var} ≈ {y0_star:.2f}**으로 **{extremum_type}**"
                    )
                    st.markdown(
                        f"- {dummy_var}=1일 경우 **{var1} ≈ {x1_star:.2f}, {var2} ≈ {x2_star:.2f}**에서 **{dep_var} ≈ {y1_star:.2f}**으로 **{extremum_type}**"
                    )
                    if in_range:
                        st.markdown(
                            f"- 현재 설정한 설명변수 범위**안에 반환점 존재**"
                        )
                    else:
                        st.markdown(
                            f"- 현재 설정한 설명변수 범위**밖에 반환점 존재**"
                        )
                    st.caption("팁: 반환점 근처로 x 범위를 좁히면 곡률이 더 선명하게 보일 수 있어요")

    elif model_type == "상호작용항 모형":

        if len(dummy_vars) == 0 and len(base_vars) == 2:
            var1 = base_vars[0]
            var2 = base_vars[1]
            X1 = np.linspace(*ranges[var1], 50)
            X2 = np.linspace(*ranges[var2], 50)
            X1_grid, X2_grid = np.meshgrid(X1, X2)

            Z = np.full_like(X1_grid, intercept)
            Z += coeffs.get(var1, 0)*X1_grid + coeffs.get(var2, 0)*X2_grid

            for term, (v1, v2) in interaction_mapping.items():
                if {v1, v2} == {var1, var2}:
                    Z += coeffs.get(term, 0) * X1_grid * X2_grid

            fig = plt.figure(constrained_layout=True)
            ax = fig.add_subplot(111, projection='3d')
            ax.plot_surface(X1_grid, X2_grid, Z, cmap='viridis', alpha=0.8)
            ax.set_xlabel(var1)
            ax.set_ylabel(var2)
            ax.text2D(
                1.02, 0.84,
                dep_var,
                transform=ax.transAxes,
                fontsize=10,
                rotation=0,
                ha='right', va='top',
                color='black'
            )

            if r_squared is not None:
                info_text = f"R²: {r_squared:.3f}"
            else:
                info_text = ""
            ax.text2D(0.05, 0.95, info_text, transform=ax.transAxes,
                      fontsize=10, ha='left', va='top',
                      bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))

            ax.set_title(f"Regression result")
            st.pyplot(fig)
            st.download_button(
                "PNG 다운로드",
                data=fig_to_bytes(fig, fmt="png", dpi=300),
                file_name=f"{dep_var}_plot.png",
                mime="image/png"
            )


        elif len(dummy_vars) == 1 and len(base_vars) == 1:
            var1 = base_vars[0]
            dummy = dummy_vars[0]
            inter_term = interaction_vars[0]

            X = np.linspace(*ranges[var1], 100)

            Y0 = intercept + coeffs.get(var1, 0) * X
            Y1 = (
                intercept
                + coeffs.get(dummy, 0)
                + (coeffs.get(var1, 0) + coeffs.get(inter_term, 0)) * X
            )

            fig, ax = plt.subplots()
            ax.plot(X, Y0, label=f"{dummy}=0", color="blue")
            ax.plot(X, Y1, label=f"{dummy}=1", color="orange")

            ax.set_xlabel(var1)
            ax.set_ylabel(dep_var)
            ax.set_title("Regression result")
            ax.legend()
            if r_squared is not None:
                info_text = f"R²: {r_squared:.3f}"
            else:
                info_text = ""
            ax.text(0.05, 0.95, info_text, transform=ax.transAxes,
                    fontsize=10, ha='left', va='top',
                    bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))

            z_min_auto = min(np.min(Y0), np.min(Y1))
            z_max_auto = max(np.max(Y0), np.max(Y1))
            z_span = z_max_auto - z_min_auto
            z_margin = min(0.05 * z_span, 1)

            if z_max_user > z_min_user:
                ax.set_ylim(z_min_user - z_margin, z_max_user + z_margin)
            else:
                ax.set_ylim(z_min_auto - z_margin, z_max_auto + z_margin)

            st.pyplot(fig)
            st.download_button(
                "PNG 다운로드",
                data=fig_to_bytes(fig, fmt="png", dpi=300),
                file_name=f"{dep_var}_plot.png",
                mime="image/png"
            )

        elif len(dummy_vars) == 1 and len(base_vars) == 2:
            var1 = base_vars[0]
            var2 = base_vars[1]
            x1 = np.linspace(*ranges[var1], 50)
            x2 = np.linspace(*ranges[var2], 50)
            X1_grid, X2_grid = np.meshgrid(x1, x2)
            dummy = dummy_vars[0] if dummy_vars else None

            Z0 = np.full_like(X1_grid, intercept)
            Z1 = np.full_like(X1_grid, intercept + coeffs.get(dummy, 0))

            Z0 += coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid
            Z1 += coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid

            terms = list(interaction_mapping.keys())
            n = len(terms)

            if n == 1:
                term = interaction_vars[0]
                v1, v2 = interaction_mapping[term]

                if dummy in (v1, v2):
                    other = v1 if v2 == dummy else v2
                    if other == var1:
                        Z1 += coeffs.get(term, 0) * X1_grid
                    else:
                        Z1 += coeffs.get(term, 0) * X2_grid

                else:
                    Z0 += coeffs.get(term, 0) * X1_grid * X2_grid
                    Z1 += coeffs.get(term, 0) * X1_grid * X2_grid

            elif n == 2:
                dummy_terms = []
                nondummy_terms = []

                for term in interaction_vars:
                    v1, v2 = interaction_mapping[term]
                    if dummy in (v1, v2):
                        dummy_terms.append((term, v1, v2))
                    else:
                        nondummy_terms.append((term, v1, v2))

                if len(dummy_terms) == 1 and len(nondummy_terms) == 1:
                    for term, v1, v2 in dummy_terms:
                        other = (set([v1, v2]) - {dummy}).pop()
                        if other == var1:
                            Z1 += coeffs.get(term, 0) * X1_grid
                        elif other == var2:
                            Z1 += coeffs.get(term, 0) * X2_grid
                        
                    for term, v1, v2 in nondummy_terms:
                        Z0 += coeffs.get(term, 0) * X1_grid * X2_grid
                        Z1 += coeffs.get(term, 0) * X1_grid * X2_grid

                elif len(dummy_terms) == 2 and len(nondummy_terms) == 0:
                    for term, v1, v2 in dummy_terms:
                        other = (set([v1, v2]) - {dummy}).pop()
                        if other == var1:
                            Z1 += coeffs.get(term, 0) * X1_grid
                        elif other == var2:
                            Z1 += coeffs.get(term, 0) * X2_grid

            elif n == 3:
                dummy_terms = []
                nondummy_terms = []

                for term in interaction_vars:
                    v1, v2 = interaction_mapping[term]
                    if dummy in (v1, v2):
                        dummy_terms.append((term, v1, v2))
                    else:
                        nondummy_terms.append((term, v1, v2))
                
                for term, v1, v2 in dummy_terms:
                    other = (set([v1, v2]) - {dummy}).pop()
                    if other == var1:
                        Z1 += coeffs.get(term, 0) * X1_grid
                    elif other == var2:
                        Z1 += coeffs.get(term, 0) * X2_grid

                for term, v1, v2 in nondummy_terms:
                    Z0 += coeffs.get(term, 0) * X1_grid * X2_grid
                    Z1 += coeffs.get(term, 0) * X1_grid * X2_grid
            
            fig = plt.figure(constrained_layout=True)
            ax = fig.add_subplot(111, projection='3d')

            ax.plot_surface(X1_grid, X2_grid, Z0, cmap='Blues', alpha=0.6, label=f'{dummy}=0')
            ax.plot_surface(X1_grid, X2_grid, Z1, cmap='Oranges', alpha=0.6, label=f'{dummy}=1')

            legend_elements = [
                Line2D([0], [0], marker='o', color='w', label=f'{dummy}=0',
                    markerfacecolor='blue', markersize=7),
                Line2D([0], [0], marker='o', color='w', label=f'{dummy}=1',
                    markerfacecolor='orange', markersize=7),
            ]

            ax.set_xlabel(var1)
            ax.set_ylabel(var2)
            ax.text2D(
                1.02, 0.84,
                dep_var,
                transform=ax.transAxes,
                fontsize=10,
                rotation=0,
                ha='right', va='top',
                color='black'
            )
            if r_squared is not None:
                info_text = f"R²: {r_squared:.3f}"
            else:
                info_text = ""
            ax.text2D(0.05, 0.95, info_text, transform=ax.transAxes,
                      fontsize=10, ha='left', va='top',
                      bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))

            ax.set_title("Regression result")
            ax.legend(handles=legend_elements, fontsize=9, handletextpad=0.1)
            st.pyplot(fig)
            st.download_button(
                "PNG 다운로드",
                data=fig_to_bytes(fig, fmt="png", dpi=300),
                file_name=f"{dep_var}_plot.png",
                mime="image/png"
            )

    elif model_type == "제곱항 + 상호작용항 모형":
        if len(dummy_vars) == 1 and len(base_vars) == 1:
            var1 = base_vars[0]
            dummy = dummy_vars[0] if dummy_vars else None
            inter_term = interaction_vars[0]
            var_sq = squared_vars[0]

            X = np.linspace(*ranges[var1], 100)
            Y0 = intercept + coeffs.get(var1, 0) * X
            Y1 = intercept + coeffs.get(dummy, 0) + coeffs.get(var1, 0) * X + coeffs.get(inter_term, 0) * X

            Y0 += coeffs.get(var_sq, 0) * (X ** 2)
            Y1 += coeffs.get(var_sq, 0) * (X ** 2)

            fig, ax = plt.subplots()
            ax.plot(X, Y0, label=f"{dummy}=0", color="blue")
            ax.plot(X, Y1, label=f"{dummy}=1", color="orange")

            ax.set_xlabel(var1)
            ax.set_ylabel(dep_var)
            if r_squared is not None:
                info_text = f"R²: {r_squared:.3f}"
            else:
                info_text = ""
            ax.text(0.05, 0.95, info_text, transform=ax.transAxes,
                    fontsize=10, ha='left', va='top',
                    bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))

            ax.set_title("Regression result")
            ax.legend()

            z_min_auto = min(np.min(Y0), np.min(Y1))
            z_max_auto = max(np.max(Y0), np.max(Y1))
            z_span = z_max_auto - z_min_auto
            z_margin = min(0.05 * z_span, 1)

            if z_max_user > z_min_user:
                ax.set_ylim(z_min_user - z_margin, z_max_user + z_margin)
            else:
                ax.set_ylim(z_min_auto - z_margin, z_max_auto + z_margin)

            st.pyplot(fig)
            st.download_button(
                "PNG 다운로드",
                data=fig_to_bytes(fig, fmt="png", dpi=300),
                file_name=f"{dep_var}_plot.png",
                mime="image/png"
            )

        elif len(dummy_vars) == 0 and len(base_vars) == 2:
            var1 = base_vars[0]
            var2 = base_vars[1]
            inter_term = interaction_vars[0]

            x1 = np.linspace(*ranges[var1], 50)
            x2 = np.linspace(*ranges[var2], 50)
            X1_grid, X2_grid = np.meshgrid(x1, x2)

            Z = intercept + coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid + coeffs.get(inter_term, 0) * X1_grid * X2_grid
            for sq_var, base in squared_mapping.items():
                if base == var1:
                    Z += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                elif base == var2:
                    Z += coeffs.get(sq_var, 0) * (X2_grid ** 2)

            fig = plt.figure(constrained_layout=True)
            ax = fig.add_subplot(111, projection='3d')
            ax.plot_surface(X1_grid, X2_grid, Z, cmap='viridis', alpha=0.8)

            ax.set_xlabel(var1)
            ax.set_ylabel(var2)
            ax.text2D(
                1.02, 0.84,
                dep_var,
                transform=ax.transAxes,
                fontsize=10,
                rotation=0,
                ha='right', va='top',
                color='black'
            )
            if r_squared is not None:
                info_text = f"R²: {r_squared:.3f}"
            else:
                info_text = ""
            ax.text2D(0.05, 0.95, info_text, transform=ax.transAxes,
                      fontsize=10, ha='left', va='top',
                      bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))

            ax.set_title("Regression result")
            st.pyplot(fig)
            st.download_button(
                "PNG 다운로드",
                data=fig_to_bytes(fig, fmt="png", dpi=300),
                file_name=f"{dep_var}_plot.png",
                mime="image/png"
            )

        elif len(dummy_vars) == 1 and len(base_vars) == 2:
            if len(squared_vars) == 1 and len(interaction_vars) == 1:
                var1 = base_vars[0]
                var2 = base_vars[1]
                term = interaction_vars[0]
                v1, v2 = interaction_mapping[term]
                dummy = dummy_vars[0] if dummy_vars else None

                x1 = np.linspace(*ranges[var1], 50)
                x2 = np.linspace(*ranges[var2], 50)
                X1_grid, X2_grid = np.meshgrid(x1, x2)

                Z0 = intercept + coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid
                Z1 = intercept + coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid + coeffs.get(dummy, 0)

                for sq_var, base in squared_mapping.items():
                    if base == var1:
                        Z0 += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                        Z1 += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                    elif base == var2:
                        Z0 += coeffs.get(sq_var, 0) * (X2_grid ** 2)
                        Z1 += coeffs.get(sq_var, 0) * (X2_grid ** 2)

                if dummy in (v1, v2):
                    other = v1 if v2 == dummy else v2
                    if other == var1:
                        Z1 += coeffs.get(term, 0) * X1_grid
                    else:
                        Z1 += coeffs.get(term, 0) * X2_grid

                else:
                    Z0 += coeffs.get(term, 0) * X1_grid * X2_grid
                    Z1 += coeffs.get(term, 0) * X1_grid * X2_grid

                fig = plt.figure(constrained_layout=True)
                ax = fig.add_subplot(111, projection='3d')

                ax.plot_surface(X1_grid, X2_grid, Z0, cmap='Blues', alpha=0.6, label=f'{dummy}=0')
                ax.plot_surface(X1_grid, X2_grid, Z1, cmap='Oranges', alpha=0.6, label=f'{dummy}=1')

                legend_elements = [
                    Line2D([0], [0], marker='o', color='w', label=f'{dummy}=0',
                        markerfacecolor='blue', markersize=7),
                    Line2D([0], [0], marker='o', color='w', label=f'{dummy}=1',
                        markerfacecolor='orange', markersize=7),
                ]

                ax.set_xlabel(var1)
                ax.set_ylabel(var2)
                ax.text2D(
                    1.02, 0.84,
                    dep_var,
                    transform=ax.transAxes,
                    fontsize=10,
                    rotation=0,
                    ha='right', va='top',
                    color='black'
                )
                if r_squared is not None:
                    info_text = f"R²: {r_squared:.3f}"
                else:
                    info_text = ""
                ax.text2D(0.05, 0.95, info_text, transform=ax.transAxes,
                          fontsize=10, ha='left', va='top',
                          bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))

                ax.set_title("Regression result")
                ax.legend(handles=legend_elements, fontsize=9, handletextpad=0.1)
                st.pyplot(fig)
                st.download_button(
                    "PNG 다운로드",
                    data=fig_to_bytes(fig, fmt="png", dpi=300),
                    file_name=f"{dep_var}_plot.png",
                    mime="image/png"
                )

            elif len(squared_vars) == 1 and len(interaction_vars) == 2:
                var1 = base_vars[0]
                var2 = base_vars[1]
                dummy_terms = []
                nondummy_terms = []
                dummy = dummy_vars[0] if dummy_vars else None

                x1 = np.linspace(*ranges[var1], 50)
                x2 = np.linspace(*ranges[var2], 50)
                X1_grid, X2_grid = np.meshgrid(x1, x2)

                Z0 = intercept + coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid
                Z1 = intercept + coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid + coeffs.get(dummy, 0)

                for sq_var, base in squared_mapping.items():
                    if base == var1:
                        Z0 += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                        Z1 += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                    elif base == var2:
                        Z0 += coeffs.get(sq_var, 0) * (X2_grid ** 2)
                        Z1 += coeffs.get(sq_var, 0) * (X2_grid ** 2)

                for term in interaction_vars:
                    v1, v2 = interaction_mapping[term]
                    if dummy in (v1, v2):
                        dummy_terms.append((term, v1, v2))
                    else:
                        nondummy_terms.append((term, v1, v2))

                if len(dummy_terms) == 1 and len(nondummy_terms) == 1:
                    for term, v1, v2 in dummy_terms:
                        other = (set([v1, v2]) - {dummy}).pop()
                        if other == var1:
                            Z1 += coeffs.get(term, 0) * X1_grid
                        elif other == var2:
                            Z1 += coeffs.get(term, 0) * X2_grid

                    for term, v1, v2 in nondummy_terms:
                        Z0 += coeffs.get(term, 0) * X1_grid * X2_grid
                        Z1 += coeffs.get(term, 0) * X1_grid * X2_grid

                elif len(dummy_terms) == 2 and len(nondummy_terms) == 0:
                    for term, v1, v2 in dummy_terms:
                        other = (set([v1, v2]) - {dummy}).pop()
                        if other == var1:
                            Z1 += coeffs.get(term, 0) * X1_grid
                        elif other == var2:
                            Z1 += coeffs.get(term, 0) * X2_grid

                fig = plt.figure(constrained_layout=True)
                ax = fig.add_subplot(111, projection='3d')

                ax.plot_surface(X1_grid, X2_grid, Z0, cmap='Blues', alpha=0.6, label=f'{dummy}=0')
                ax.plot_surface(X1_grid, X2_grid, Z1, cmap='Oranges', alpha=0.6, label=f'{dummy}=1')

                legend_elements = [
                    Line2D([0], [0], marker='o', color='w', label=f'{dummy}=0',
                        markerfacecolor='blue', markersize=7),
                    Line2D([0], [0], marker='o', color='w', label=f'{dummy}=1',
                        markerfacecolor='orange', markersize=7),
                ]

                ax.set_xlabel(var1)
                ax.set_ylabel(var2)
                ax.text2D(
                    1.02, 0.84,
                    dep_var,
                    transform=ax.transAxes,
                    fontsize=10,
                    rotation=0,
                    ha='right', va='top',
                    color='black'
                )
                if r_squared is not None:
                    info_text = f"R²: {r_squared:.3f}"
                else:
                    info_text = ""
                ax.text2D(0.05, 0.95, info_text, transform=ax.transAxes,
                          fontsize=10, ha='left', va='top',
                          bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))

                ax.set_title("Regression result")
                ax.legend(handles=legend_elements, fontsize=9, handletextpad=0.1)
                st.pyplot(fig)
                st.download_button(
                    "PNG 다운로드",
                    data=fig_to_bytes(fig, fmt="png", dpi=300),
                    file_name=f"{dep_var}_plot.png",
                    mime="image/png"
                )

            elif len(squared_vars) == 1 and len(interaction_vars) == 3:
                var1 = base_vars[0]
                var2 = base_vars[1]
                dummy = dummy_vars[0] if dummy_vars else None
                dummy_terms = []
                nondummy_terms = []

                x1 = np.linspace(*ranges[var1], 50)
                x2 = np.linspace(*ranges[var2], 50)
                X1_grid, X2_grid = np.meshgrid(x1, x2)

                Z0 = intercept + coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid
                Z1 = intercept + coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid + coeffs.get(dummy, 0)

                for sq_var, base in squared_mapping.items():
                    if base == var1:
                        Z0 += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                        Z1 += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                    elif base == var2:
                        Z0 += coeffs.get(sq_var, 0) * (X2_grid ** 2)
                        Z1 += coeffs.get(sq_var, 0) * (X2_grid ** 2)

                for term in interaction_vars:
                    v1, v2 = interaction_mapping[term]
                    if dummy in (v1, v2):
                        dummy_terms.append((term, v1, v2))
                    else:
                        nondummy_terms.append((term, v1, v2))

                for term, v1, v2 in dummy_terms:
                        other = (set([v1, v2]) - {dummy}).pop()
                        if other == var1:
                            Z1 += coeffs.get(term, 0) * X1_grid
                        elif other == var2:
                            Z1 += coeffs.get(term, 0) * X2_grid

                for term, v1, v2 in nondummy_terms:
                        Z0 += coeffs.get(term, 0) * X1_grid * X2_grid
                        Z1 += coeffs.get(term, 0) * X1_grid * X2_grid

                fig = plt.figure(constrained_layout=True)
                ax = fig.add_subplot(111, projection='3d')

                ax.plot_surface(X1_grid, X2_grid, Z0, cmap='Blues', alpha=0.6, label=f'{dummy}=0')
                ax.plot_surface(X1_grid, X2_grid, Z1, cmap='Oranges', alpha=0.6, label=f'{dummy}=1')

                legend_elements = [
                    Line2D([0], [0], marker='o', color='w', label=f'{dummy}=0',
                        markerfacecolor='blue', markersize=7),
                    Line2D([0], [0], marker='o', color='w', label=f'{dummy}=1',
                        markerfacecolor='orange', markersize=7),
                ]

                ax.set_xlabel(var1)
                ax.set_ylabel(var2)
                ax.text2D(
                    1.02, 0.84,
                    dep_var,
                    transform=ax.transAxes,
                    fontsize=10,
                    rotation=0,
                    ha='right', va='top',
                    color='black'
                )
                if r_squared is not None:
                    info_text = f"R²: {r_squared:.3f}"
                else:
                    info_text = ""
                ax.text2D(0.05, 0.95, info_text, transform=ax.transAxes,
                          fontsize=10, ha='left', va='top',
                          bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))
                ax.set_title("Regression result")
                ax.legend(handles=legend_elements, fontsize=9, handletextpad=0.1)
                st.pyplot(fig)
                st.download_button(
                    "PNG 다운로드",
                    data=fig_to_bytes(fig, fmt="png", dpi=300),
                    file_name=f"{dep_var}_plot.png",
                    mime="image/png"
                )

            elif len(squared_vars) == 2 and len(interaction_vars) == 1:
                var1 = base_vars[0]
                var2 = base_vars[1]
                dummy = dummy_vars[0] if dummy_vars else None
                term = interaction_vars[0]
                v1, v2 = interaction_mapping[term]

                x1 = np.linspace(*ranges[var1], 50)
                x2 = np.linspace(*ranges[var2], 50)
                X1_grid, X2_grid = np.meshgrid(x1, x2)

                Z0 = intercept + coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid
                Z1 = intercept + coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid + coeffs.get(dummy, 0)

                for sq_var, base in squared_mapping.items():
                    if base == var1:
                        Z0 += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                        Z1 += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                    elif base == var2:
                        Z0 += coeffs.get(sq_var, 0) * (X2_grid ** 2)
                        Z1 += coeffs.get(sq_var, 0) * (X2_grid ** 2)

                if dummy in (v1, v2):
                    other = v1 if v2 == dummy else v2
                    if other == var1:
                        Z1 += coeffs.get(term, 0) * X1_grid
                    else:
                        Z1 += coeffs.get(term, 0) * X2_grid

                else:
                    Z0 += coeffs.get(term, 0) * X1_grid * X2_grid
                    Z1 += coeffs.get(term, 0) * X1_grid * X2_grid

                fig = plt.figure(constrained_layout=True)
                ax = fig.add_subplot(111, projection='3d')

                ax.plot_surface(X1_grid, X2_grid, Z0, cmap='Blues', alpha=0.6, label=f'{dummy}=0')
                ax.plot_surface(X1_grid, X2_grid, Z1, cmap='Oranges', alpha=0.6, label=f'{dummy}=1')

                legend_elements = [
                    Line2D([0], [0], marker='o', color='w', label=f'{dummy}=0',
                        markerfacecolor='blue', markersize=7),
                    Line2D([0], [0], marker='o', color='w', label=f'{dummy}=1',
                        markerfacecolor='orange', markersize=7),
                ]

                ax.set_xlabel(var1)
                ax.set_ylabel(var2)
                ax.text2D(
                    1.02, 0.84,
                    dep_var,
                    transform=ax.transAxes,
                    fontsize=10,
                    rotation=0,
                    ha='right', va='top',
                    color='black'
                )
                if r_squared is not None:
                    info_text = f"R²: {r_squared:.3f}"
                else:
                    info_text = ""
                ax.text2D(0.05, 0.95, info_text, transform=ax.transAxes,
                          fontsize=10, ha='left', va='top',
                          bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))
                ax.set_title("Regression result")
                ax.legend(handles=legend_elements, fontsize=9, handletextpad=0.1)
                st.pyplot(fig)
                st.download_button(
                    "PNG 다운로드",
                    data=fig_to_bytes(fig, fmt="png", dpi=300),
                    file_name=f"{dep_var}_plot.png",
                    mime="image/png"
                )

            elif len(squared_vars) == 2 and len(interaction_vars) == 2:
                var1 = base_vars[0]
                var2 = base_vars[1]
                dummy = dummy_vars[0] if dummy_vars else None
                dummy_terms = []
                nondummy_terms = []

                x1 = np.linspace(*ranges[var1], 50)
                x2 = np.linspace(*ranges[var2], 50)
                X1_grid, X2_grid = np.meshgrid(x1, x2)

                Z0 = intercept + coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid
                Z1 = intercept + coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid + coeffs.get(dummy, 0)

                for sq_var, base in squared_mapping.items():
                    if base == var1:
                        Z0 += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                        Z1 += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                    elif base == var2:
                        Z0 += coeffs.get(sq_var, 0) * (X2_grid ** 2)
                        Z1 += coeffs.get(sq_var, 0) * (X2_grid ** 2)

                for term in interaction_vars:
                    v1, v2 = interaction_mapping[term]
                    if dummy in (v1, v2):
                        dummy_terms.append((term, v1, v2))
                    else:
                        nondummy_terms.append((term, v1, v2))

                if len(dummy_terms) == 1 and len(nondummy_terms) == 1:
                    for term, v1, v2 in dummy_terms:
                        other = (set([v1, v2]) - {dummy}).pop()
                        if other == var1:
                            Z1 += coeffs.get(term, 0) * X1_grid
                        elif other == var2:
                            Z1 += coeffs.get(term, 0) * X2_grid

                    for term, v1, v2 in nondummy_terms:
                        Z0 += coeffs.get(term, 0) * X1_grid * X2_grid
                        Z1 += coeffs.get(term, 0) * X1_grid * X2_grid

                elif len(dummy_terms) == 2 and len(nondummy_terms) == 0:
                    for term, v1, v2 in dummy_terms:
                        other = (set([v1, v2]) - {dummy}).pop()
                        if other == var1:
                            Z1 += coeffs.get(term, 0) * X1_grid
                        elif other == var2:
                            Z1 += coeffs.get(term, 0) * X2_grid

                fig = plt.figure(constrained_layout=True)
                ax = fig.add_subplot(111, projection='3d')

                ax.plot_surface(X1_grid, X2_grid, Z0, cmap='Blues', alpha=0.6, label=f'{dummy}=0')
                ax.plot_surface(X1_grid, X2_grid, Z1, cmap='Oranges', alpha=0.6, label=f'{dummy}=1')

                legend_elements = [
                    Line2D([0], [0], marker='o', color='w', label=f'{dummy}=0',
                        markerfacecolor='blue', markersize=7),
                    Line2D([0], [0], marker='o', color='w', label=f'{dummy}=1',
                        markerfacecolor='orange', markersize=7),
                ]

                ax.set_xlabel(var1)
                ax.set_ylabel(var2)
                ax.text2D(
                    1.02, 0.84,
                    dep_var,
                    transform=ax.transAxes,
                    fontsize=10,
                    rotation=0,
                    ha='right', va='top',
                    color='black'
                )
                if r_squared is not None:
                    info_text = f"R²: {r_squared:.3f}"
                else:
                    info_text = ""
                ax.text2D(0.05, 0.95, info_text, transform=ax.transAxes,
                          fontsize=10, ha='left', va='top',
                          bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))
                ax.set_title("Regression result")
                ax.legend(handles=legend_elements, fontsize=9, handletextpad=0.1)
                st.pyplot(fig)
                st.download_button(
                    "PNG 다운로드",
                    data=fig_to_bytes(fig, fmt="png", dpi=300),
                    file_name=f"{dep_var}_plot.png",
                    mime="image/png"
                )

            elif len(squared_vars) == 2 and len(interaction_vars) == 3:
                var1 = base_vars[0]
                var2 = base_vars[1]
                dummy = dummy_vars[0] if dummy_vars else None
                dummy_terms = []
                nondummy_terms = []

                x1 = np.linspace(*ranges[var1], 50)
                x2 = np.linspace(*ranges[var2], 50)
                X1_grid, X2_grid = np.meshgrid(x1, x2)

                Z0 = intercept + coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid
                Z1 = intercept + coeffs.get(var1, 0) * X1_grid + coeffs.get(var2, 0) * X2_grid + coeffs.get(dummy, 0)

                for sq_var, base in squared_mapping.items():
                    if base == var1:
                        Z0 += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                        Z1 += coeffs.get(sq_var, 0) * (X1_grid ** 2)
                    elif base == var2:
                        Z0 += coeffs.get(sq_var, 0) * (X2_grid ** 2)
                        Z1 += coeffs.get(sq_var, 0) * (X2_grid ** 2)

                for term in interaction_vars:
                    v1, v2 = interaction_mapping[term]
                    if dummy in (v1, v2):
                        dummy_terms.append((term, v1, v2))
                    else:
                        nondummy_terms.append((term, v1, v2))

                for term, v1, v2 in dummy_terms:
                        other = (set([v1, v2]) - {dummy}).pop()
                        if other == var1:
                            Z1 += coeffs.get(term, 0) * X1_grid
                        elif other == var2:
                            Z1 += coeffs.get(term, 0) * X2_grid

                for term, v1, v2 in nondummy_terms:
                        Z0 += coeffs.get(term, 0) * X1_grid * X2_grid
                        Z1 += coeffs.get(term, 0) * X1_grid * X2_grid

                fig = plt.figure(constrained_layout=True)
                ax = fig.add_subplot(111, projection='3d')

                ax.plot_surface(X1_grid, X2_grid, Z0, cmap='Blues', alpha=0.6, label=f'{dummy}=0')
                ax.plot_surface(X1_grid, X2_grid, Z1, cmap='Oranges', alpha=0.6, label=f'{dummy}=1')

                legend_elements = [
                    Line2D([0], [0], marker='o', color='w', label=f'{dummy}=0',
                        markerfacecolor='blue', markersize=7),
                    Line2D([0], [0], marker='o', color='w', label=f'{dummy}=1',
                        markerfacecolor='orange', markersize=7),
                ]

                ax.set_xlabel(var1)
                ax.set_ylabel(var2)
                ax.text2D(
                    1.02, 0.84,
                    dep_var,
                    transform=ax.transAxes,
                    fontsize=10,
                    rotation=0,
                    ha='right', va='top',
                    color='black'
                )
                if r_squared is not None:
                    info_text = f"R²: {r_squared:.3f}"
                else:
                    info_text = ""
                ax.text2D(0.05, 0.95, info_text, transform=ax.transAxes,
                          fontsize=10, ha='left', va='top',
                          bbox=dict(boxstyle="round", facecolor="white", alpha=0.7))
                ax.set_title("Regression result")
                ax.legend(handles=legend_elements, fontsize=9, handletextpad=0.1)
                st.pyplot(fig)
                st.download_button(
                    "PNG 다운로드",
                    data=fig_to_bytes(fig, fmt="png", dpi=300),
                    file_name=f"{dep_var}_plot.png",
                    mime="image/png"
                )

    st.divider()
    st.subheader(" 계량경제의 기초 ")

    with st.expander("좋은 추정량에 대한 논의", expanded=False):
        st.markdown("### 비편향성과 효율성")
        st.markdown(
        """
        - 자료로부터 계산된 추정량은 모수를 추정하는 데 활용되는데, 그렇다면 모수를 최대한 적은 시도 하에서 최대한 근접하게 추정하는 추정량이 좋은 추정량이라 할 수 있다.
        - 이를 위해서는 추정량의 비편향성이 성립함과 동시에 BLUE가 충족되어야 한다.(BLUE : Best Linear Unbiased Estimator, 가우스-마코프 정리에 의해 증명)
        - 비편향성은 표본으로부터 계산된 추정량의 평균이 모수와 동일함을 의미하며, BLUE는 모수를 추정하는 데 활용되는 선형 비편향 추정량 중 해당 추정량이 가장 분산이 적은 추정량임을 의미하고 이는 가장 효율적인 추정량임을 의미한다.
        """
    )

    with st.expander("좋은 모형에 대한 논의", expanded=False):
        st.markdown("### 설명력: R²")
        st.markdown(
        r"""
        - 연구자가 회귀모형을 상정하고 회귀분석을 수행했을 때 연구자가 상정한 회귀모형이 적합한 모형인지, 좋은 모형인지 판단할 필요가 있다.
        - 좋은 모형이라 함은 설명변수가 종속변수를 잘 설명하는 모형일 것이다. 잘 설명한다는 것은, 회귀선 상의 맞춘값들과 실제 관측값들과의 차이가 적음을 의미한다.
        - 이를 설명력이라는 이름을 붙여 다음과 같은 식을 통해 정량화한다.
        $$
        R^2 = \frac{SSE}{SST},\ SST = \sum_{i=1}^{n} (y_i - \bar{y})^2,\ SSE = \sum_{i=1}^{n} (\hat{y}_i - \bar{y})^2
        $$
        """
    )

    with st.expander("오차에 대한 논의", expanded=False):
        st.markdown("### 오차에 관한 기본가정")
        st.markdown(
        """
        - 논의에 앞서, 현재 설문조사를 통해 데이터를 수집한 상황이며 설문조사의 특성상 어느 한 질문에서 응답자가 고른 선택지별로 데이터를 구획화하는 것이 가능함을 인지한다.
        - 또한, 설문조사를 계속해서 1회차, 2회차, ... , N회차 반복하면 해당 데이터 구획에 여러 회차별 자료가 속해있다고 생각할 수 있으며, 이 데이터에서 오차 또한 도출된다. 이때 다음의 네 가지 가정이 성립한다.
        - 오차평균 0 가정 : 모든 구획에서 종속변수와 맞춘값의 차이인 오차의 평균이 0이라는 가정이다. 해당 가정은 OLS추정량의 비편향성과 BLUE를 증명하는데 활용된다.
        - 오차 간 독립추출 가정 : 오차들은 서로 어떠한 상관관계도 없이, 독립적으로 추출되었음을 의미한다. OLS추정량이 BLUE임을 증명하는데 활용된다.
        - 오차 간 동일분산 가정 : 모든 구획에서 오차들의 분산이 동일함을 의미한다. OLS추정량이 BLUE임을 증명하는데 활용된다.
        - 오차의 정규분포 가정 : 오차들은 정규분포를 이루는 모집단으로부터 추출되며, 오차 또한 정규분포를 따른다. 해당 가정이 성립할 경우 OLS추정량은 모든 비편향 추정량 중에서 가장 효율적인 추정량이 된다.
        """
    )

    with st.expander("검정에 대한 논의", expanded=False):
        st.markdown("### 통계검정")
        st.markdown(
        r"""
        - t통계량 : t통계량은 다음과 같이 계산된다.
        $$
        t = \frac{\hat{\beta}_j - {\beta}_j}{se(\hat{\beta}_j)}
        $$
        - F통계량 : F통계량은 다음과 같이 계산된다.
        $$
        F = \frac{(SSR_R - SSR_U)/m}{s^2},\ s^2=\frac{SSR_U}{n-k-1},\ m: \text{귀무가설 식의 개수}
        $$
        - T검정 : 기초적인 검정 방법이며, 단일한 식으로 표현되는 귀무가설을 검정하는 데 활용할 수 있다. 정규분포와 마찬가지로 표준화를 거쳐 나온 검정통계량(t통계량)을 기반으로 귀무가설의 기각여부를 판단한다.
        - F검정 : 여러 식으로 표현되는 귀무가설을 검정할 수 있는 검정 방법이며, 귀무가설의 제약 하에서 구해진 잔차제곱합과 제약이 존재하지 않는 잔차제곱합의 차이를 활용해 귀무가설의 기각여부를 판단한다. 다만, 부등호의 형태로 표현되는 대립가설은 검정이 불가하다.
        - 기본적으로 계량경제학에서 통계 검정은 설명변수와 종속변수 간의 관계가 유의미한지를 파악하기 위함에 있다. 이때 귀무가설은 $H_0:{\beta}_j=0$이며, 이 귀무가설을 정해진 유의수준 하에서 기각할 수 있다면 연구자는 해당 유의수준 하에서 설명변수가 종속변수와 관계가 없지 않다고 판단할 수 있다.
        - 또한, 귀무가설의 형태는 생각보다 자유롭게 활용할 수 있다. 기울기 추정량과 구체적인 수치의 곱으로 표현된 선형제약식도 모수변환을 통해 검정할 수 있으며, 이는 예측값에 대한 검정으로 활용할 수 있다.
        """
    )

    with st.expander("변화에 대한 논의", expanded=False):
        st.markdown("### 자연로그의 활용")
        st.markdown(
        r"""
        - 자연로그는 신기한 특성을 가지고 있다. 자연로그의 변화분이 충분히 작을 때(0.1이하) 로그 내부의 변수의 변화율과 로그의 변화분이 근사한다는 것이다.
        - 이 성질을 회귀모형 분석에 활용할 수 있다.
        - 로그-로그 모형 : $x$가 1% 변화할 때 $y$는 $\beta_1$% 변화한다. 
        $$
        \ln(y) = \beta_0 + \beta_1 ln(x) + u
        $$
        - 로그-수준 모형 : $x$가 1단위 변화할 때 $y$는 $100 \beta_1$% 변화한다.
        $$
        \ln(y) = \beta_0 + \beta_1 x + u
        $$
        - 수준-로그 모형 : $x$가 1% 변화할 때 $y$는 $0.01 \beta_1$만큼 변화한다.
        $$
        y = \beta_0 + \beta_1 ln(x) + u
        $$
        - 수준-수준 모형 : $x$가 1단위 변화할 때 $y$는 $\beta_1$만큼 변화한다.
        $$
        y = \beta_0 + \beta_1 x + u
        $$
        """
    )

    with st.expander("모형에 대한 논의", expanded=False):
        st.markdown("### 특이항의 활용")
        st.markdown(
        """
        - 일반적으로, 연구자가 모형을 설정할 때 활용하는 설명변수는 일차항이며, 연속형 변수이다.
        - 그러나 상황에 따라 변수로 더미변수, 상호작용항, 제곱항을 활용할 수 있다.
        - 더미변수 : 더미변수는 0과 1의 값만을 갖는 변수다. 더미변수는 남,여 같은 상호배타적인 두 집단으로 구분되는 집단에 대해 회귀분석을 할 경우 활용할 수 있다.
        - 상호작용항 : 두 설명변수의 곱으로 이루어진 변수이다. 두 변수가 서로 영향을 주고받아 종속변수에 추가적인 영향을 줄 것으로 예상될 경우 활용할 수 있다. 
        - 제곱항 : 설명변수와 종속변수 간의 관계가 비선형일 때 활용할 수 있다. 예를 들면, bmi지수가 높은 것도 건강에 좋지 않지만 낮은 것도 건강에 좋지 않아 의료비가 높을 수 있다.
        """
    )
