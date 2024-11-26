%Code to export cue tables and reference times into csv format

%Params
input_dir = '/Volumes/cc23_eds/CCAS_hyena_2023_auditing/data/cue tables';
output_dir = '/Volumes/cc23_eds/cc23_cue_tables_csv';

%go to directory with the cue tables
cd(input_dir)

%list files in the directory
files = dir('*/_cc23_*wavcues.mat');

for i = 1:length(files)

    %go into the directory
    cd(files(i).folder);

    %name of the matlab file
    matname = files(i).name;

    %load the cue table
    load(matname);

    %name base
    basename = extractBefore(matname,".");

    %csv file name
    csvname = strcat(basename, '.csv');

    %reference time filename
    ref_time_name = strcat(basename,'_ref_time.csv');

    %go into the output directory
    cd(output_dir)
    writematrix(cuetab, csvname);
    
    writematrix(ref_time, ref_time_name);

end


