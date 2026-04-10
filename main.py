import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from logic import run_simulation, calculate_hunnlus_prob
from imr_api import get_initial_config, find_location_id, get_infection_pressure, parse_wkt_point, get_pressure_for_week

st.set_page_config(page_title="Lusekalkulator", layout="wide")

# Norwegian to English Translations for UI
T = {
    "title": "Lusekalkulator (Lice Calculator)",
    "infection_pressure": "Infection Pressure (Smittepress)",
    "auto_pressure": "Automatic (from HI database)",
    "manual_pressure": "Manual",
    "loc_search": "Search for Farm (Name or Number)",
    "pressure_val": "Infection Pressure Value",
    "start_from": "Start calculation from",
    "year": "Year",
    "week": "Week (1-52)",
    "strategy": "Treatment Strategy",
    "whole_site": "Whole Site",
    "merd_wise": "Cage-wise (Merdvis)",
    "data_source": "Cage Data Source",
    "upload_csv": "Upload CSV file",
    "manual_data": "Enter data manually",
    "calculate": "Calculate Development",
    "lice": "Lice (Mobile)",
    "weight": "Weight (kg)",
    "cleaner_fish": "Cleaner Fish",
    "attached": "Attached Lice",
    "yes": "Yes",
    "no": "No",
    "results": "Results",
    "weeks_ahead": "Weeks Ahead",
    "lice_per_fish": "Lice per fish per cage",
    "prob_limit": "Prob > 0.5 female lice",
}

@st.cache_data
def load_hi_config():
    return get_initial_config()

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

        sm_mode = st.radio(T["infection_pressure"], ["auto", "manual"], index=0)

        selected_farm = None
        if sm_mode == "auto":
            selected_farm = st.selectbox(T["loc_search"], farm_list)

        col1, col2 = st.columns(2)
        year = col1.number_input(T["year"], min_value=2012, max_value=2030, value=max_year)
        week = col2.number_input(T["week"], min_value=1, max_value=53, value=default_week)

        auto_ln_smp = None
        if sm_mode == "auto" and selected_farm:
            internal_id, wkt = find_location_id(selected_farm)
            if internal_id:
                lat, lon = parse_wkt_point(wkt)
                ts = get_infection_pressure(lat, lon, year)
                pressure = get_pressure_for_week(ts, week)
                if pressure is not None:
                    auto_ln_smp = np.log(pressure + 1)
                    st.success(f"Infection pressure found: {auto_ln_smp:.4f}")
                else:
                    st.error(f"No infection pressure data found for {selected_farm} in week {week}, {year}.")
            else:
                st.error("Could not find site in HI database.")

        if sm_mode == "manual":
            ln_smp = st.number_input(T["pressure_val"], min_value=0.0, max_value=22.0, value=15.0)
        else:
            ln_smp = auto_ln_smp if auto_ln_smp is not None else None

        st.divider()
        strategy = st.radio(T["strategy"], ["whole", "merdvis"], format_func=lambda x: T["whole_site"] if x=="whole" else T["merd_wise"])
        data_mode = st.radio(T["data_source"], ["csv", "manual"])

        input_data = []
        if data_mode == "csv":
            uploaded_file = st.file_uploader(T["upload_csv"], type="csv")
            if uploaded_file:
                try:
                    df = pd.read_csv(uploaded_file, sep=";", decimal=",")
                    input_data = df.values.tolist()
                except Exception as e:
                    st.error(f"Error reading CSV: {e}")
        else:
            num_cages = st.number_input("Number of cages", min_value=1, max_value=16, value=1)
            for i in range(num_cages):
                st.subheader(f"Cage {i+1}")
                c1, c2, c3, c4 = st.columns(4)
                l = c1.number_input(f"{T['lice']} ##{i+1}", value=0.0, step=0.1, key=f"l_{i}")
                w = c2.number_input(f"{T['weight']} ##{i+1}", value=0.0, step=0.1, key=f"w_{i}")
                cf = c3.selectbox(f"{T['cleaner_fish']} ##{i+1}", [0, 1], format_func=lambda x: T["yes"] if x==1 else T["no"], key=f"cf_{i}")
                a = c4.selectbox(f"{T['attached']} ##{i+1}", [0, 1], format_func=lambda x: T["yes"] if x==1 else T["no"], key=f"a_{i}")
                input_data.append([l, w, cf, a])

    if st.button(T["calculate"]):
        if sm_mode == "auto" and ln_smp is None:
            st.error("Cannot calculate: Valid automatic infection pressure is missing for the selected site/week.")
            return

        if not input_data:
            st.warning("Please provide input data.")
            return

        # Prepare data for simulation
        merder_m1 = [row[0] for row in input_data]
        vekt_merd = [row[1] for row in input_data]
        re_fis = [row[2] for row in input_data]
        l_m1 = [row[3] for row in input_data]

        with st.spinner("Running simulation..."):
            results = run_simulation(merder_m1, vekt_merd, re_fis, l_m1, ln_smp, whole_site=(strategy=="whole"))

        st.header(T["results"])

        n_cages = len(merder_m1)
        for i in range(0, n_cages, 4):
            cols = st.columns(4)
            for j in range(4):
                idx = i + j
                if idx < n_cages:
                    with cols[j]:
                        fig, ax = plt.subplots()
                        xtid = [0, 1, 2, 3]
                        means = [results[w]['means'][idx] for w in range(4)]
                        ax.plot(xtid, means, color='red', linewidth=2, label='Expected')

                        q25 = [results[w]['q25'][idx] for w in range(1, 4)]
                        q75 = [results[w]['q75'][idx] for w in range(1, 4)]
                        ax.plot(xtid[1:], q25, color='black', linestyle='--')
                        ax.plot(xtid[1:], q75, color='black', linestyle='--')

                        ax.set_title(f"Cage {idx+1}")
                        ax.set_xlabel(T["weeks_ahead"])
                        ax.set_ylabel(T["lice_per_fish"])
                        ax.set_ylim(0, max(7, max(means)*1.2))

                        p1 = calculate_hunnlus_prob(results[1]['means'][idx])
                        p2 = calculate_hunnlus_prob(results[2]['means'][idx])
                        p3 = calculate_hunnlus_prob(results[3]['means'][idx])

                        st.pyplot(fig)
                        st.write(f"**{T['prob_limit']}:**")
                        st.write(f"W1: {p1:.1f}% | W2: {p2:.1f}% | W3: {p3:.1f}%")

if __name__ == "__main__":
    main()
