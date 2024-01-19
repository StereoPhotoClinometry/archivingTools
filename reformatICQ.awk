# 17 Jan 2024
# John R. Weirich
# Reformats ICQ to be fixed width table
# Be sure to change D to E before running the script
# You can use "sed '2,$ s/D/E/g' SHAPE.TXT > tmp" to do this


NR==1 {
	printf("%-80s\n", $0)
}

NR!=1 {
	printf("%18.9f %18.9f %18.9f %18.9f\n", $1, $2, $3, $4)
}
