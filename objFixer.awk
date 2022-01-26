# Eric E. Palmer - 26 Jan 2022
# Takes an OBJ file and converts it into a fixed width table
#      It will put the floating point values (real) into a 
#          wide enough field so that everything is aligned
#          even when the body is larger than 1,000 km
#      It pads the vertix values to handle Q sizes bigger than 
#          1024 (12M verticies) 
#      Based upon the output of PLT2MAP of the AltWG software
# Designed to be used within OLAF fixed width tables
/v/ { printf ("%c %25.14f %25.14f %25.14f\n", $1, $2, $3, $4) }
/f/ { printf ("%c %9d %9d %9d\n",  $1, $2, $3, $4 ) }
