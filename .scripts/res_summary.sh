#!/bin/bash
#
# Very basic script to collect all .resinfo files and print the files
# that consumed the most memory and CPU time. You should run the
# makefile with RESOURCEMONITOR=1 to generate the .resinfo files.

declare -A cpu
declare -A mem

printAll=false

if [ $# -gt 0 ] && ( [ $1 == "--all" ] || [ $1 == "-a" ] ); then
	printAll=true
	shift
fi

# Traverse all .resinfo files and store the relevant information in the
# associative arrays above.
for f in $(find . -name '*.resinfo'); do
	fp=${f/.resinfo/}

	s=$(grep group.mempeak $f | grep -Eo '[0-9.KMG]*B')
	s=${s/GB/000MB}
	s=${s/MB/000KB}
	s=${s/KB/}
	mem[$fp]=$s

	t=$(grep group.total $f | grep -Eo '[0-9.]+s')
	t=${t/s/}
	cpu[$fp]=$t
done

# If -a/--all was given, print a line for each file.
if $printAll; then
	echo "All space and time:"
	for fp in "${!mem[@]}"; do
		printf "RESMON: %-80s %12s %12s\n" "$fp" "${cpu[$fp]}s" "${mem[$fp]}KB"
	done
fi

# Print the top 20 in memory and CPU time.
echo "Top 20 memory:"
for fp in "${!mem[@]}"; do
	printf " %-80s %12s\n" "$fp" "${mem[$fp]} KB"
done | sort -k2 -n -r  | head -n 20
echo

echo "Top 20 CPU time:"
for fp in "${!cpu[@]}"; do
	printf " %-80s %12s\n" "$fp" "${cpu[$fp]} s"
done | sort -k2 -n -r  | head -n 20
echo

TOTMEM=0
TOTCPU=0
# Trying to do this in the loops above won't work as the command runs in
# a subshell, with its own set of variables. Bash is fun :^).
for fp in "${!mem[@]}"; do
	TOTMEM=$(echo $TOTMEM + ${mem[$fp]} | bc)
	TOTCPU=$(echo $TOTCPU + ${cpu[$fp]} | bc)
done

echo "Total CPU: $TOTCPU seconds"
echo "Total memory: $TOTMEM KB"
