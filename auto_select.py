from glob import glob
from pathlib import Path
import subprocess
import sys

import geopandas as gpd
import pandas as pd


def run_projection_2010(ssp: str):
    subprocess.check_call(
        ['Rscript', '--vanilla', 'script_SpatialModel_projection_2010.R', ssp],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )


def run_projection_post_2010(ssp: str, decade: int):
    subprocess.check_call(
        [
            'Rscript', '--vanilla', 'script_SpatialModel_projection_2020&on.R',
            ssp,
            str(decade)
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )


def run_covariate_update(ssp: str, year: int):
    subprocess.check_call(
        ['python', 'script_SpatialModel_projection_CovariateUpdates.py', ssp, f'{year}'],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )


def csv_to_dbf(ssp: str, decade: int):
    year = 2000 + decade * 10
    filename = f'tbl_inputToArcGIS_{ssp}_{year}.csv'
    data = gpd.GeoDataFrame(
        pd.read_csv(filename).drop(['Unnamed: 0'], axis=1)
    )
    data['geometry'] = None
    data.to_file(f'tbl_inputAttr_{ssp}_{year}.dbf', index=False)


def shp_to_csv(ssp: str, decade: int):
    year = 2000 + decade * 10
    new_r = gpd.read_file('_tempCentroidnewR.dbf', ignore_geometry=True).drop(['AreaGoed', 'ISO', 'ORIG_FID'], axis=1).set_index('originFID').sort_index()
    new_d = gpd.read_file('_tempCentroidnewD.dbf', ignore_geometry=True).drop(['AreaGoed', 'ISO', 'ORIG_FID'], axis=1).set_index('originFID').sort_index()
    new_a = gpd.read_file('_tempCentroidnewA.dbf', ignore_geometry=True).drop(['AreaGoed', 'ISO', 'ORIG_FID'], axis=1).set_index('originFID').sort_index()
    new_r.join(new_d).join(new_a).reset_index().to_csv(
        f'temp_tbl_attr_updates_{ssp}_{year}.csv',
        index=False
    )


def ssp_to_tif(ssp: str, output_path):
    grid = gpd.read_file(
        'LandMask_1-8-degree_DATA/LandMask_1-8-degree_fishnet_centroids.shp'
    ).set_index('originFID')[['geometry']]
    files = sorted(glob(f'tbl_inputToArcGIS_{ssp}_*.csv'))
    Path(output_path).mkdir(parents=True, exist_ok=True)
    for f in files:
        year = f[-8:-4]
        filename = f'urban_fraction_{ssp}_{year}.shp'
        urban = grid.join(pd.read_csv(f).set_index('originFID')[['newR']]).rename(
            columns={'newR': 'urban_frac'}
        )
        urban.to_file(f'{output_path}/{filename}')
        output_filename = f'{output_path}/{ssp}/urban_fraction_{ssp}_{year}.tif'
        Path(output_filename).parent.mkdir(parents=True, exist_ok=True)
        subprocess.check_call(
            [
                'gdal_rasterize',
                '-l', filename[:-4],
                '-a', 'urban_frac',
                '-tr', '0.125', '0.125',
                '-te', '-180', '-90', '180', '90',
                '-ot', 'Float32',
                '-of', 'GTiff',
                f'{output_path}/{filename}',
                output_filename,
            ],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )


def downscale(ssp: str, output_path: str, start_decade: int = None):
    subprocess.check_call(
        [
            'python',
            'script_Downscaling_OneSSP2000-2100.py',
            ssp,
            str(Path(output_path).absolute()),
            start_decade if start_decade is not None else ''
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )


def auto_select(ssp: str, start_decade: int = 1, output_path: str = './output'):
    print(f'Running auto_select for {ssp}...')
    if start_decade == 1:
        print(f'  Projecting to 2010.')
        run_projection_2010(ssp)
    for decade in range(start_decade, 10):
        year = 2000 + 10 * decade
        print(f'  Converting to .dbf')
        csv_to_dbf(ssp, decade)
        print(f'  Updating covariates')
        run_covariate_update(ssp, year)
        print(f'  Converting to .csv')
        shp_to_csv(ssp, decade)
        print(f'  Projecting to {year + 10}')
        run_projection_post_2010(ssp, decade + 1)
    print('  Converting to .tif')
    ssp_to_tif(ssp, output_path)
    print('  Downscaling')
    downscale(ssp, output_path)
    print('Done.')


if __name__ == '__main__':

    if len(sys.argv) < 2:
        exit()

    my_ssp = sys.argv[1].upper()
    if my_ssp not in ['SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5']:
        exit()

    if len(sys.argv) > 2:
        begin_decade = int(sys.argv[2])
    else:
        begin_decade = 1

    auto_select(my_ssp, begin_decade)
