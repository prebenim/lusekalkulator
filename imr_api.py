import requests
from datetime import datetime

BASE_URL = "https://www.hi.no/forskning/marine-data-forskningsdata/lakseluskart"

def get_initial_config():
    """Fetches the initial configuration from HI."""
    try:
        response = requests.get(f"{BASE_URL}/initialConfig", timeout=10)
        response.raise_for_status()
        return response.json()
    except Exception as e:
        print(f"Error fetching HI config: {e}")
        return None

def find_location_id(name_number):
    """Gets the internal GUID for a farm name or number."""
    try:
        response = requests.post(
            f"{BASE_URL}/fishFarmLocation",
            data={"nameNumber": name_number},
            timeout=10
        )
        response.raise_for_status()
        data = response.json()
        if data:
            return data[0]['id'], data[0]['locationWKT']
        return None, None
    except Exception as e:
        print(f"Error finding location: {e}")
        return None, None

def get_infection_pressure(lat, lon, year):
    """Fetches infection pressure timeseries for a coordinate and year."""
    try:
        response = requests.get(
            f"{BASE_URL}/smittepressTimeSeriesByYear",
            params={"lat": lat, "lon": lon, "year": year},
            timeout=10
        )
        response.raise_for_status()
        return response.json()
    except Exception as e:
        print(f"Error fetching infection pressure: {e}")
        return None

def parse_wkt_point(wkt):
    """Simple parser for POINT(lon lat)."""
    if not wkt:
        return None, None
    try:
        parts = wkt.replace("POINT(", "").replace(")", "").split()
        return float(parts[1]), float(parts[0]) # lat, lon
    except:
        return None, None

def get_pressure_for_week(timeseries, week):
    """
    Extracts the pressure value for a specific week from the timeseries.
    The timeseries is a list of [timestamp, value, year_string].
    """
    if not timeseries:
        return None

    # Sort by timestamp just in case
    # Convert timestamp to week number
    for entry in timeseries:
        ts = entry[0] / 1000 # to seconds
        dt = datetime.fromtimestamp(ts)
        # dt.isocalendar().week
        if dt.isocalendar()[1] == week:
            return entry[1]
    return None
