#!/bin/sh

prog=`cat <<EOF
/^module /,/where/ { next; }
/^( *--|import)/ { next; }
//
EOF`

total=0

for i in `find Dedukti Dedukti.hs -name '*.hs' -or -name '*.lhs'`
do
    count=`awk "$prog" $i | wc -l`
    total=$((total+count))
    echo $i : $count
done

echo Total: $total
