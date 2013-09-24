"""Usage: prep_data.py CONFIG

"""
import json
import os

import pandas as pd

from collections import OrderedDict
from docopt import docopt

def read_patient_data(filename):
    col_map = OrderedDict([
        ('PtID' , 'patient_id'),
        ('DOB' , 'date_of_birth'),
        ('Sex' , 'sex'),
        ('Race1' , 'race1'),
        ('Race2' , 'race2'),
        ('ethnic' , 'ethnicity'),
        ('DateFirstSeen' , 'date_first_seen'),
        ('DateDiagnosis' , 'date_diagnosed'),
        ('Date1stSymptom' , 'date_first_symptom'),
        ('ingSmokStatus' , 'smoker'),
    ])

    df = pd.read_csv(filename)
    df = df[col_map.keys()]
    df = df.rename(columns = col_map)
    df.index = pd.Index(df['patient_id'])
    return df

def read_clinic_data(filename):
    col_map = OrderedDict([
        ('PtID' , 'patient_id'),
        ('Visit.Date' , 'date'),
        ('Total.Skin.Score' , 'total_skin'),
        ('Skin.Sev.Score', 'skin_severity'),
        ('RP.Sev.Score', 'rp_severity'),
        ('GI.Sev.Score', 'gi_severity'),
    ])

    df = pd.read_csv(filename)
    df = df[col_map.keys()]
    df = df.rename(columns = col_map)
    return df

def read_pft_data(filename):
    col_map = OrderedDict([
        ('PtID' , 'patient_id'),
        ('Date' , 'date'),
        ('Height' , 'height'),
        ('Weight', 'weight'),
        ('age', 'age'),
        ('FVC.Pre', 'fvc'),
        ('perc.FVC.of.predicted', 'pfvc'),
        ('FEV1.Pre', 'fev1'),
        ('perc.FEV1.of.predicted', 'pfev1'),
        ('TLC_HE', 'tlc'),
        ('perc.TLC.of.predicted', 'ptlc'),
        ('DLCO', 'dlco'),
        ('perc.DLCO.of.predicted', 'pdlco'),
    ])

    df = pd.read_csv(filename)
    df = df[col_map.keys()]
    df = df.rename(columns = col_map)
    return df

def read_sero_data(filename):
    col_map = OrderedDict([
        ('PtID' , 'patient_id'),
        ('Sample.Date' , 'date'),
        ('ANA' , 'ana'),
        ('ACA' , 'aca'),
        ('Scl.70' , 'scl.70'),
        ('Ro' , 'ro'),
        ('La' , 'la'),
        ('RNP' , 'rnp'),
        ('Sm' , 'sm'),
        ('DNA' , 'dna'),
        ('RNA.Polymerase.I.III' , 'rna.poly'),
    ])

    df = pd.read_csv(filename)
    df = df[col_map.keys()]
    df = df.rename(columns = col_map)
    return df

def main():
    args = docopt(__doc__)

    with open(args['CONFIG']) as f:
        config_map = json.load(f)

    sclero_data = config_map['sclero_datadir']
    local_data = config_map['data_input']

    if not os.path.exists(local_data):
        os.makedirs(local_data)

    def get_in_out(data_key):
        d_in = os.path.join(sclero_data, config_map[data_key]["in"])
        d_out = os.path.join(local_data, config_map[data_key]["out"])

        return d_in, d_out

    patient_in, patient_out = get_in_out("patient")
    read_patient_data(patient_in).to_csv(patient_out, index = False)

    clinic_in, clinic_out = get_in_out("clinic")
    read_clinic_data(clinic_in).to_csv(clinic_out, index = False)

    pft_in, pft_out = get_in_out("pft")
    read_pft_data(pft_in).to_csv(pft_out, index = False)

    sero_in, sero_out = get_in_out("sero")
    read_sero_data(sero_in).to_csv(sero_out, index = False)

if __name__ == '__main__':
    main()
