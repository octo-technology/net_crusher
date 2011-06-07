#!/bin/sh -e

NODE_NAME=$1
INPUT_FILE=$2
OUTPUT_FILE=$3
SCALE=$4

BASE=`dirname $0`

#[Slab, SwapCached, SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, Inactive]
TITLE="Memory for $NODE_NAME (bytes)"
RRD=/tmp/tmp.rrd
SIZE=`tail -n 1 $INPUT_FILE | awk '{print int($1)}'`
echo "Scale for $TITLE : $SCALE s"
RRA="RRA:MIN:0.5:$SCALE:3600 RRA:MAX:0.5:$SCALE:3600 RRA:AVERAGE:0.5:$SCALE:3600"
DS="DS:slab:GAUGE:20:0:U DS:swap_cached:GAUGE:20:0:U DS:swap_total:GAUGE:20:0:U DS:swap_free:GAUGE:20:0:U DS:page_tables:GAUGE:20:0:U DS:vmalloc_used:GAUGE:20:0:U DS:mem_total:GAUGE:20:0:U DS:mem_free:GAUGE:20:0:U DS:buffers:GAUGE:20:0:U DS:cached:GAUGE:20:0:U DS:committed:GAUGE:20:0:U DS:mapped:GAUGE:20:0:U DS:active:GAUGE:20:0:U DS:inactive:GAUGE:20:0:U"
rrdtool create $RRD --step 1 $DS $RRA
echo RRD created
TIME=1420070400
MAX=`tail -n 1 $INPUT_FILE | awk '{print $8}'`
AWK_CMD="{print int(\$1) + $TIME \":\" \$2 \":\" \$3 \":\" \$4 \":\" \$5 \":\" \$6 \":\" \$7 \":\" \$8 \":\" \$9 \":\" \$10 \":\" \$11 \":\" \$12 \":\" \$13 \":\" \$14 \":\" \$15 \" \"}"
cat $INPUT_FILE | grep -v "^-" | awk "$AWK_CMD" | xargs rrdtool update $RRD
echo RRD filled 
rrdtool graph $OUTPUT_FILE \
\
DEF:slab=$RRD:slab:AVERAGE \
DEF:swap_cached=$RRD:swap_cached:AVERAGE \
DEF:swap_total=$RRD:swap_total:AVERAGE \
DEF:swap_free=$RRD:swap_free:AVERAGE \
DEF:page_tables=$RRD:page_tables:AVERAGE \
DEF:vmalloc_used=$RRD:vmalloc_used:AVERAGE \
DEF:mem_total=$RRD:mem_total:AVERAGE \
DEF:mem_free=$RRD:mem_free:AVERAGE \
DEF:buffers=$RRD:buffers:AVERAGE \
DEF:cached=$RRD:cached:AVERAGE \
DEF:committed=$RRD:committed:AVERAGE \
DEF:mapped=$RRD:mapped:AVERAGE \
DEF:active=$RRD:active:AVERAGE \
DEF:inactive=$RRD:inactive:AVERAGE \
CDEF:swap=swap_total,swap_free,- \
CDEF:s1=mem_total,mem_free,- \
CDEF:s2=s1,buffers,- \
CDEF:s3=s2,cached,- \
CDEF:s4=s3,slab,- \
CDEF:s5=s4,page_tables,- \
CDEF:apps=s5,swap_cached,- \
\
AREA:apps`$BASE/../get_color.sh 1`:"Apps         " \
GPRINT:apps:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:apps:MAX:"Maximum\:%8.2lf %s" \
GPRINT:apps:MIN:"Minimum\:%8.2lf %s\n" \
\
STACK:page_tables`$BASE/../get_color.sh 2`:"Page tables  " \
GPRINT:page_tables:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:page_tables:MAX:"Maximum\:%8.2lf %s" \
GPRINT:page_tables:MIN:"Minimum\:%8.2lf %s\n" \
\
STACK:swap_cached`$BASE/../get_color.sh 3`:"Swap cache   " \
GPRINT:swap_cached:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:swap_cached:MAX:"Maximum\:%8.2lf %s" \
GPRINT:swap_cached:MIN:"Minimum\:%8.2lf %s\n" \
\
STACK:slab`$BASE/../get_color.sh 4`:"Slab cache   " \
GPRINT:slab:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:slab:MAX:"Maximum\:%8.2lf %s" \
GPRINT:slab:MIN:"Minimum\:%8.2lf %s\n" \
\
STACK:cached`$BASE/../get_color.sh 5`:"Cache        " \
GPRINT:cached:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:cached:MAX:"Maximum\:%8.2lf %s" \
GPRINT:cached:MIN:"Minimum\:%8.2lf %s\n" \
\
STACK:buffers`$BASE/../get_color.sh 6`:"Buffers      " \
GPRINT:buffers:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:buffers:MAX:"Maximum\:%8.2lf %s" \
GPRINT:buffers:MIN:"Minimum\:%8.2lf %s\n" \
\
STACK:mem_free`$BASE/../get_color.sh 7`:"Unused       " \
GPRINT:mem_free:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:mem_free:MAX:"Maximum\:%8.2lf %s" \
GPRINT:mem_free:MIN:"Minimum\:%8.2lf %s\n" \
\
STACK:swap`$BASE/../get_color.sh 8`:"Swap         " \
GPRINT:swap:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:swap:MAX:"Maximum\:%8.2lf %s" \
GPRINT:swap:MIN:"Minimum\:%8.2lf %s\n" \
\
LINE2:vmalloc_used`$BASE/../get_color.sh 9`:"Vmalloc used " \
GPRINT:vmalloc_used:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:vmalloc_used:MAX:"Maximum\:%8.2lf %s" \
GPRINT:vmalloc_used:MIN:"Minimum\:%8.2lf %s\n" \
\
LINE2:committed`$BASE/../get_color.sh 10`:"Committed    " \
GPRINT:committed:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:committed:MAX:"Maximum\:%8.2lf %s" \
GPRINT:committed:MIN:"Minimum\:%8.2lf %s\n" \
\
LINE2:mapped`$BASE/../get_color.sh 11`:"Mapped       " \
GPRINT:mapped:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:mapped:MAX:"Maximum\:%8.2lf %s" \
GPRINT:mapped:MIN:"Minimum\:%8.2lf %s\n" \
\
LINE2:active`$BASE/../get_color.sh 12`:"Active       " \
GPRINT:active:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:active:MAX:"Maximum\:%8.2lf %s" \
GPRINT:active:MIN:"Minimum\:%8.2lf %s\n" \
\
LINE2:inactive`$BASE/../get_color.sh 13`:"Inactive     " \
GPRINT:inactive:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:inactive:MAX:"Maximum\:%8.2lf %s" \
GPRINT:inactive:MIN:"Minimum\:%8.2lf %s\n" \
\
--lower-limit 0 \
--upper-limit $MAX \
--rigid \
--slope-mode \
--title "$TITLE" \
--width 700 \
--height 300 \
--start $TIME \
--end start+$SIZE \
--x-grid SECOND:5:MINUTE:2:SECOND:30:0:%M:%S
