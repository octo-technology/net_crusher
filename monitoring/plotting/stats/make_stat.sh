#!/bin/sh -e

NODE_NAME=$1
INPUT_FILE=$2
OUTPUT_FILE=$3
SCALE=$4

BASE=`dirname $0`

TITLE="CPU for $NODE_NAME (used %%)"
RRD=/tmp/tmp.rrd
SIZE=`tail -n 1 $INPUT_FILE | awk '{print int($1)}'`
echo "Scale for $TITLE : $SCALE s"
RRA="RRA:MIN:0.5:$SCALE:3600 RRA:MAX:0.5:$SCALE:3600 RRA:AVERAGE:0.5:$SCALE:3600"
DS="DS:user:COUNTER:20:0:U DS:nice:COUNTER:20:0:U DS:system:COUNTER:20:0:U DS:idle:COUNTER:20:0:U DS:iowait:COUNTER:20:0:U DS:irq:COUNTER:20:0:U DS:softirq:COUNTER:20:0:U DS:steal:COUNTER:20:0:U"
rrdtool create $RRD --step 1 $DS $RRA
echo RRD created
TIME=1420070400
AWK_CMD="{print int(\$1) + $TIME \":\" \$2 \":\" \$3 \":\" \$4 \":\" \$5 \":\" \$6 \":\" \$7 \":\" \$8 \":\" \$9 \" \"}"
cat $INPUT_FILE | grep -v "^-" | awk "$AWK_CMD" | xargs rrdtool update $RRD
echo RRD filled 
rrdtool graph $OUTPUT_FILE \
\
DEF:user=$RRD:user:AVERAGE \
DEF:nice=$RRD:nice:AVERAGE \
DEF:system=$RRD:system:AVERAGE \
DEF:iowait=$RRD:iowait:AVERAGE \
DEF:irq=$RRD:irq:AVERAGE \
DEF:softirq=$RRD:softirq:AVERAGE \
DEF:steal=$RRD:steal:AVERAGE \
\
AREA:user`$BASE/../get_color.sh 1`:"User   " \
GPRINT:user:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:user:MAX:"Maximum\:%8.2lf %s" \
GPRINT:user:MIN:"Minimum\:%8.2lf %s\n" \
\
STACK:nice`$BASE/../get_color.sh 2`:"Nice   " \
GPRINT:nice:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:nice:MAX:"Maximum\:%8.2lf %s" \
GPRINT:nice:MIN:"Minimum\:%8.2lf %s\n" \
\
STACK:system`$BASE/../get_color.sh 3`:"System " \
GPRINT:system:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:system:MAX:"Maximum\:%8.2lf %s" \
GPRINT:system:MIN:"Minimum\:%8.2lf %s\n" \
\
STACK:iowait`$BASE/../get_color.sh 4`:"Iowait " \
GPRINT:iowait:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:iowait:MAX:"Maximum\:%8.2lf %s" \
GPRINT:iowait:MIN:"Minimum\:%8.2lf %s\n" \
\
STACK:irq`$BASE/../get_color.sh 5`:"Irq    " \
GPRINT:irq:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:irq:MAX:"Maximum\:%8.2lf %s" \
GPRINT:irq:MIN:"Minimum\:%8.2lf %s\n" \
\
STACK:softirq`$BASE/../get_color.sh 6`:"SoftIrq" \
GPRINT:softirq:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:softirq:MAX:"Maximum\:%8.2lf %s" \
GPRINT:softirq:MIN:"Minimum\:%8.2lf %s\n" \
\
STACK:steal`$BASE/../get_color.sh 7`:"Steal  " \
GPRINT:steal:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:steal:MAX:"Maximum\:%8.2lf %s" \
GPRINT:steal:MIN:"Minimum\:%8.2lf %s\n" \
\
--slope-mode \
--title "$TITLE" \
--width 700 \
--height 300 \
--start $TIME \
--end start+$SIZE \
--x-grid SECOND:5:MINUTE:2:SECOND:30:0:%M:%S
