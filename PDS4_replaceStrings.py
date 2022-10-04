#!/usr/bin/env python
import os, fnmatch, shutil

#method to backup files before running findReplace()
def findAndBackup(directory, filePattern):
    for path, dirs, files in os.walk(os.path.abspath(directory)):
        for filename in fnmatch.filter(files, filePattern):
            filepath = os.path.join(path, filename)
            backup = filepath + ".bak"
            shutil.copy (filepath, backup)
            print (filepath + " => " + backup)

#send two arrays for string find and replace
def findReplace(directory, find_array, replace_array, filePattern):
    for path, dirs, files in os.walk(os.path.abspath(directory)):
        for filename in fnmatch.filter(files, filePattern):
            filepath = os.path.join(path, filename)
            with open(filepath) as f:
                s = f.read()
            for i in range(len(find_array)):
                s = s.replace(find_array[i], replace_array[i])
                #replace XFILENAMEX if found
                file_noExt = os.path.basename(filename).split('.')[0]
                file_noExt = file_noExt.lower()
                s = s.replace("XFILENAMEX",file_noExt)
            with open(filepath, "w") as f:
                f.write(s)

def main():

    #optional to run. Best to backup or zip whole directory though
    #    findAndBackup(".","*.xml")

    #initialize empty arrays
    in_str=[]
    re_str=[]

    #update unique logical identifier
    #urn:nasa:pds:body_mission_instrument_type_author_year:data:vevft201_lot
    #Note the XFILENAMEX will get replaced above in recursive loop with the filename (no extension)
    in_str.append("""${LOGICAL_IDENTIFIER}""")
    re_str.append("""urn:nasa:pds:lunar_lro_lroc_topography_domingue_2022:data:XFILENAMEX""")

    #update investigation area 
    in_str.append("""${INVESTIGATION_AREA_NAME}""")
    re_str.append("""Lunar Reconnaissance Orbiter""")

    #update investigation area lid reference
    in_str.append("""${INVESTIGATION_AREA_LID_REFERENCE}""")
    re_str.append("""urn:nasa:pds:context:instrument_host:spacecraft.lro""")

    #update Observing_System_Component. Also currently GDAL defaults to "spacecraft" 
    # but I think "instrument" would be better here and matches the observing system name used.
    in_str.append("""${OBSERVING_SYSTEM_NAME}""")
    re_str.append("""Lunar Reconnaissance Orbiter Camera""")
    # part 2 of this replacement
    in_str.append("""<type>Spacecraft</type>""")
    re_str.append("""<type>Instrument</type>""")

    #update several sections, lower and upper case Moon_2000, ${TARGET_TYPE}, #${target_type}

    #update TARGET_TYPE and lowercase target_type (annoying their is a lowercase version). 
    in_str.append("""Moon_2000""")
    re_str.append("""Moon""")
    #part 2 of the replacement
    in_str.append("""${TARGET_TYPE}""")
    re_str.append("""Satellite""")
    #part 3 of the replacement, lower case
    in_str.append("""moon_2000""")
    re_str.append("""earth.moon""")
    # part 4 lower case
    in_str.append("""${target_type}""")
    re_str.append("""satellite""")

    #actually run the findReplace fundtion passing the arrays
    findReplace(".",in_str,re_str,"*.xml")

if __name__ == "__main__":
    main()

