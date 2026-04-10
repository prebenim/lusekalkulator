import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from logic import run_simulation, calculate_dT
from imr_api import get_initial_config, find_location_id, get_infection_pressure, parse_wkt_point, get_pressure_for_week

st.set_page_config(page_title="Lusekalkulator", layout="wide")

# Norwegian to English Translations for UI
T = {
    "title": "Lusekalkulator (Lice Calculator) - Stige et al. (2024) Model",
    "infection_pressure": "Infection Pressure (Smittepress)",
    "auto_pressure": "Automatic (from HI database)",
    "manual_pressure": "Manual",
    "loc_search": "Search for Farm (Name or Number)",
    "pressure_val": "Infection Pressure Value (P)",
    "start_from": "Start calculation from",
    "year": "Year",
    "week": "Week (1-53)",
    "strategy": "Treatment Strategy",
    "whole_site": "Whole Site",
    "merd_wise": "Cage-wise (Merdvis)",
    "data_source": "Cage Data Source",
    "upload_csv": "Upload CSV file",
    "manual_data": "Enter data manually",
    "calculate": "Calculate Development",
    "sessile": "Sessile Lice (per fish)",
    "other_mobile": "Other Mobile (per fish)",
    "adult_female": "Adult Female (per fish)",
    "weight": "Fish Weight (kg)",
    "nfish": "Fish Count (thousands)",
    "cleaner_fish": "Wrasse Presence",
    "yes": "Yes",
    "no": "No",
    "results": "Results (Adult Female Prediction)",
    "weeks_ahead": "Weeks Ahead",
    "lice_per_fish": "Lice per fish",
    "prob_limit": "Prob > 0.5 female lice",
    "temperature": "Sea Temperature (°C)"
}

@st.cache_data
def load_hi_config():
    return get_initial_config()

@st.cache_data
def cached_find_location_id(selected_farm):
    return find_location_id(selected_farm)

@st.cache_data
def cached_get_infection_pressure(lat, lon, year):
    # Fetch current year and potentially previous if needed for lag
    # HI API gives whole year
    return get_infection_pressure(lat, lon, year)

def get_lagged_pressure(ts_current, ts_prev, week, year, dT):
    """Fetches pressure for week t - dT. Handles year crossover."""
    target_week_float = week - dT

    # Simple nearest week selection for now
    target_week = int(round(target_week_float))

    if target_week >= 1:
        return get_pressure_for_week(ts_current, target_week)
    else:
        # Need previous year's data
        # If we didn't fetch it, we might return None or fallback
        # For simplicity, we assume HI API provides a continuous timeseries or we fetch it
        # If ts_prev is None, we return the earliest available in current year
        if ts_prev:
            # Assuming 52 weeks in a year
            return get_pressure_for_week(ts_prev, target_week + 52)
        else:
            return get_pressure_for_week(ts_current, 1)

def main():
    st.title(T["title"])

    hi_config = load_hi_config()
    farm_list = hi_config.get("farmList", []) if hi_config else []
    max_year = hi_config.get("year", 2026) if hi_config else 2026

    # Get latest week available in smittePress-config for the current year
    default_week = 1
    if hi_config:
        weeks = [entry['week'] for entry in hi_config.get('smittePress-config', [])]
        if weeks:
            default_week = max(weeks)

    with st.sidebar:
        st.header("Settings")

        temp = st.number_input(T["temperature"], min_value=0.0, max_value=25.0, value=9.0)
        dT = calculate_dT(temp)
        st.info(f"Calculated development lag (dT): {dT:.2f} weeks")

        sm_mode = st.radio(
            T["infection_pressure"],
            ["auto", "manual"],
            index=0,
            format_func=lambda x: T["auto_pressure"] if x == "auto" else T["manual_pressure"]
        )

        selected_farm = None
        if sm_mode == "auto":
            selected_farm = st.selectbox(T["loc_search"], farm_list)

        col1, col2 = st.columns(2)
        year = col1.number_input(T["year"], min_value=2012, max_value=2030, value=max_year)
        week = col2.number_input(T["week"], min_value=1, max_value=53, value=default_week)

        auto_p_lagged = []
        if sm_mode == "auto" and selected_farm:
            internal_id, wkt = cached_find_location_id(selected_farm)
            if internal_id:
                lat, lon = parse_wkt_point(wkt)
                if lat is not None and lon is not None:
                    ts_current = cached_get_infection_pressure(lat, lon, year)
                    ts_prev = cached_get_infection_pressure(lat, lon, year - 1) if year > 2012 else None

                    # We need P for t+1-dT, t+2-dT, t+3-dT
                    valid = True
                    for w_offset in range(1, 4):
                        p = get_lagged_pressure(ts_current, ts_prev, week + w_offset, year, dT)
                        if p is not None:
                            auto_p_lagged.append(p)
                        else:
                            valid = False
                            st.error(f"Could not find lagged infection pressure for prediction week {week + w_offset}")
                            break

                    if valid:
                        st.success(f"Lagged pressures: {', '.join([f'{p:.2f}' for p in auto_p_lagged])}")
                else:
                    st.error(f"Could not parse valid coordinates for {selected_farm} from the HI database.")
            else:
                st.error("Could not find site in HI database.")

        if sm_mode == "manual":
            manual_p = st.number_input(T["pressure_val"], min_value=0.0, max_value=100.0, value=15.0)
            auto_p_lagged = [manual_p] * 3
        else:
            if not auto_p_lagged:
                auto_p_lagged = None

        st.divider()
        data_mode = st.radio(T["data_source"], ["csv", "manual"])

        input_data = []
        if data_mode == "csv":
            uploaded_file = st.file_uploader(T["upload_csv"] + " (Expected cols: S, O, A, NFISH, WFISH, WRASSE)", type="csv")
            if uploaded_file:
                try:
                    df = pd.read_csv(uploaded_file, sep=";", decimal=",")
                    input_data = df.to_dict('records')
                except Exception as e:
                    st.error(f"Error reading CSV: {e}")
        else:
            num_cages = st.number_input("Number of cages", min_value=1, max_value=16, value=1)
            for i in range(num_cages):
                st.subheader(f"Cage {i+1}")
                c1, c2, c3 = st.columns(3)
                s_l = c1.number_input(f"{T['sessile']} ##{i+1}", value=0.0, step=0.1, key=f"s_{i}")
                o_l = c2.number_input(f"{T['other_mobile']} ##{i+1}", value=0.0, step=0.1, key=f"o_{i}")
                a_l = c3.number_input(f"{T['adult_female']} ##{i+1}", value=0.0, step=0.1, key=f"a_{i}")

                c4, c5, c6 = st.columns(3)
                nf = c4.number_input(f"{T['nfish']} ##{i+1}", value=130.0, step=1.0, key=f"nf_{i}")
                wf = c5.number_input(f"{T['weight']} ##{i+1}", value=2.2, step=0.1, key=f"wf_{i}")
                wr = c6.selectbox(f"{T['cleaner_fish']} ##{i+1}", [0, 1], format_func=lambda x: T["yes"] if x==1 else T["no"], key=f"wr_{i}")

                input_data.append({
                    'S': s_l,
                    'O': o_l,
                    'A': a_l,
                    'NFISH': nf,
                    'WFISH': wf,
                    'WRASSE': wr
                })

    if st.button(T["calculate"]):
        if auto_p_lagged is None:
            st.error("Cannot calculate: Missing valid infection pressure data.")
            return

        if not input_data:
            st.warning("Please provide input data.")
            return

        with st.spinner("Running simulation..."):
            results = run_simulation(input_data, temp, auto_p_lagged)

        st.header(T["results"])

        n_cages = len(input_data)
        for i in range(0, n_cages, 4):
            cols = st.columns(4)
            for j in range(4):
                idx = i + j
                if idx < n_cages:
                    with cols[j]:
                        fig, ax = plt.subplots()
                        xtid = [0, 1, 2, 3]

                        means_a = []
                        q25_a = []
                        q75_a = []
                        probs = []

                        for w in range(4):
                            cage_res = results[w]['cages'][idx]
                            means_a.append(cage_res['mu_a'])
                            q25_a.append(cage_res['q25_a'])
                            q75_a.append(cage_res['q75_a'])
                            probs.append(cage_res['prob_exceed'])

                        ax.plot(xtid, means_a, color='red', linewidth=2, label='Expected A')
                        ax.fill_between(xtid, q25_a, q75_a, color='red', alpha=0.2, label='IQR')

                        ax.set_title(f"Cage {idx+1}")
                        ax.set_xlabel(T["weeks_ahead"])
                        ax.set_ylabel(T["lice_per_fish"])
                        ax.set_ylim(0, max(1.0, max(means_a)*1.5, max(q75_a)*1.1))
                        ax.axhline(0.5, color='gray', linestyle='--')

                        st.pyplot(fig)
                        st.write(f"**{T['prob_limit']}:**")
                        st.write(f"W1: {probs[1]:.1f}% | W2: {probs[2]:.1f}% | W3: {probs[3]:.1f}%")

if __name__ == "__main__":
    main()
