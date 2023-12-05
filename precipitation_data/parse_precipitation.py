import netCDF4
import pandas as pd
import os

latitude_values = {
    -88.75: 0,
    -86.25: 1,
    -83.75: 2,
    -81.25: 3,
    -78.75: 4,
    -76.25: 5,
    -73.75: 6,
    -71.25: 7,
    -68.75: 8,
    -66.25: 9,
    -63.75: 10,
    -61.25: 11,
    -58.75: 12,
    -56.25: 13,
    -53.75: 14,
    -51.25: 15,
    -48.75: 16,
    -46.25: 17,
    -43.75: 18,
    -41.25: 19,
    -38.75: 20,
    -36.25: 21,
    -33.75: 22,
    -31.25: 23,
    -28.75: 24,
    -26.25: 25,
    -23.75: 26,
    -21.25: 27,
    -18.75: 28,
    -16.25: 29,
    -13.75: 30,
    -11.25: 31,
    -8.75: 32,
    -6.25: 33,
    -3.75: 34,
    -1.25: 35,
    1.25: 36,
    3.75: 37,
    6.25: 38,
    8.75: 39,
    11.25: 40,
    13.75: 41,
    16.25: 42,
    18.75: 43,
    21.25: 44,
    23.75: 45,
    26.25: 46,
    28.75: 47,
    31.25: 48,
    33.75: 49,
    36.25: 50,
    38.75: 51,
    41.25: 52,
    43.75: 53,
    46.25: 54,
    48.75: 55,
    51.25: 56,
    53.75: 57,
    56.25: 58,
    58.75: 59,
    61.25: 60,
    63.75: 61,
    66.25: 62,
    68.75: 63,
    71.25: 64,
    73.75: 65,
    76.25: 66,
    78.75: 67,
    81.25: 68,
    83.75: 69,
    86.25: 70,
    88.75: 71,
}

longitude_values = {
    1.25: 0,
    3.75: 1,
    6.25: 2,
    8.75: 3,
    11.25: 4,
    13.75: 5,
    16.25: 6,
    18.75: 7,
    21.25: 8,
    23.75: 9,
    26.25: 10,
    28.75: 11,
    31.25: 12,
    33.75: 13,
    36.25: 14,
    38.75: 15,
    41.25: 16,
    43.75: 17,
    46.25: 18,
    48.75: 19,
    51.25: 20,
    53.75: 21,
    56.25: 22,
    58.75: 23,
    61.25: 24,
    63.75: 25,
    66.25: 26,
    68.75: 27,
    71.25: 28,
    73.75: 29,
    76.25: 30,
    78.75: 31,
    81.25: 32,
    83.75: 33,
    86.25: 34,
    88.75: 35,
    91.25: 36,
    93.75: 37,
    96.25: 38,
    98.75: 39,
    101.25: 40,
    103.75: 41,
    106.25: 42,
    108.75: 43,
    111.25: 44,
    113.75: 45,
    116.25: 46,
    118.75: 47,
    121.25: 48,
    123.75: 49,
    126.25: 50,
    128.75: 51,
    131.25: 52,
    133.75: 53,
    136.25: 54,
    138.75: 55,
    141.25: 56,
    143.75: 57,
    146.25: 58,
    148.75: 59,
    151.25: 60,
    153.75: 61,
    156.25: 62,
    158.75: 63,
    161.25: 64,
    163.75: 65,
    166.25: 66,
    168.75: 67,
    171.25: 68,
    173.75: 69,
    176.25: 70,
    178.75: 71,
    181.25: 72,
    183.75: 73,
    186.25: 74,
    188.75: 75,
    191.25: 76,
    193.75: 77,
    196.25: 78,
    198.75: 79,
    201.25: 80,
    203.75: 81,
    206.25: 82,
    208.75: 83,
    211.25: 84,
    213.75: 85,
    216.25: 86,
    218.75: 87,
    221.25: 88,
    223.75: 89,
    226.25: 90,
    228.75: 91,
    231.25: 92,
    233.75: 93,
    236.25: 94,
    238.75: 95,
    241.25: 96,
    243.75: 97,
    246.25: 98,
    248.75: 99,
    251.25: 100,
    253.75: 101,
    256.25: 102,
    258.75: 103,
    261.25: 104,
    263.75: 105,
    266.25: 106,
    268.75: 107,
    271.25: 108,
    273.75: 109,
    276.25: 110,
    278.75: 111,
    281.25: 112,
    283.75: 113,
    286.25: 114,
    288.75: 115,
    291.25: 116,
    293.75: 117,
    296.25: 118,
    298.75: 119,
    301.25: 120,
    303.75: 121,
    306.25: 122,
    308.75: 123,
    311.25: 124,
    313.75: 125,
    316.25: 126,
    318.75: 127,
    321.25: 128,
    323.75: 129,
    326.25: 130,
    328.75: 131,
    331.25: 132,
    333.75: 133,
    336.25: 134,
    338.75: 135,
    341.25: 136,
    343.75: 137,
    346.25: 138,
    348.75: 139,
    351.25: 140,
    353.75: 141,
    356.25: 142,
    358.75: 143,
}



def round_lat(lat):
    # range of longitude is from -88.75 to 88.75
    # with increments of 2.5 degrees
    rounded_lat = round((lat - 1.25) / 2.5) * 2.5 + 1.25
    # ensure the result is within the range -88.75 to 88.75
    return max(min(rounded_lat, 88.75), -88.75)

def round_lat2(lat):
    return round((lat - 1.25) / 2.5) * 2.5 + 1.25


def round_lon(lon):
    # range of longitude is from 1.25 to 358.75
    # with increments of 2.5 degrees
    rounded_lon = round((lon - 1.25) / 2.5) * 2.5 + 1.25
    # ensure the result is within the range 1.25 to 358.75
    return max(min(rounded_lon, 358.75), 1.25)

def round_lon2(lon):
    return round((lon - 1.25) / 2.5) * 2.5 + 1.25

outbreak_data = pd.read_csv("./outbreak_data.csv")

# round the coordinates to use with the precipitation data
df = pd.DataFrame(outbreak_data[['latitude', 'longitude', 'location_period_id']])

def round_coordinates(row):
    row['lat_rounded'] = round_lat(row['latitude'])
    row['lon_rounded'] = round_lon(row['longitude'])
    return row

#! temp
df = df.apply(round_coordinates, axis=1)

# now we have the rounded coordinates, we can use them to get the precipitation data

# 10 years (2010-2019)
# 12 months in each year

def get_precipitation(row, date):
    lat = row['lat_rounded']
    lon = row['lon_rounded']
    days_since_1970 = dataset.variables["time"][0]
    row[f"precipitation-{date}"] = dataset.variables['precip'][0][longitude_values[lon]][latitude_values[lat]]
    # raise Exception
    return row

for year in range(2010, 2020):

    files = os.listdir(f"./data/{year}")
    print(f"appending precipitation data for {year} ({len(files)} files found in directory)")
    # print(os.listdir(f"./data/{year}"))
    for (index, file) in enumerate(os.listdir(f"./data/{year}")):
        if file.find(".nc") == -1:
            continue
        if file.find("preliminary") != -1:
            continue
        dataset = netCDF4.Dataset(f"./data/{year}/{file}")

        df = df.apply(lambda row: get_precipitation(row, f"{year}_{index+1}"), axis=1)
        # break
        dataset.close()
    # break

# dataset = netCDF4.Dataset("./data/2010/gpcp_v02r03_monthly_d201001_c20170616.nc")




# df = df.apply(get_precipitation, axis=1)





df.to_csv("./outbreak_data_plus_precipitation.csv", index=False)

print(df.head())




