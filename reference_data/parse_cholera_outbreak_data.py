import pandas as pd
import geopandas as gpd




df = pd.read_csv('./reference_data/outbreak_data.csv')
gdf = gpd.read_file('./reference_data/AfricaShapefiles/total_shp_0427.shp')


# =========================================== adding longitude/latitude to outbreak data ===========================================
def add_lat_lon():
    # iterate through the rows of the dataframe
    for index, row in df.iterrows():
        # query location period id from shapefile
        rslt_df = gdf.loc[gdf['lctn_pr'] == row['location_period_id']]
        lat = rslt_df.get_coordinates().iloc[0].x
        lon = rslt_df.get_coordinates().iloc[0].y
        # now add it back to the dataframe
        df.loc[index, 'latitude'] = lat
        df.loc[index, 'longitude'] = lon


    df.to_csv('./reference_data/outbreak_data_new.csv', index=False)

# =========================================== group regions by country ===========================================

def group_regions():
    # iterate through the rows of the dataframe
    for index, row in df.iterrows():
        # query location period id from shapefile
        rslt_df = gdf.loc[gdf['lctn_pr'] == row['location_period_id']]
        country = rslt_df['country'].iloc[0]
        # now add it back to the dataframe
        df.loc[index, 'country'] = country


    df.to_csv('./reference_data/outbreak_data_new.csv', index=False)


if __name__ == '__main__':
    add_lat_lon()
    # group_regions()