#!/bin/sh -e

NODE_NAME=$1
INPUT_FILE=$2
OUTPUT_FILE=$3
SCALE=$4

BASE=`dirname $0`

TITLE="Network traffic for $NODE_NAME (byte / seconds)"
RRD=/tmp/tmp.rrd
SIZE=`tail -n 1 $INPUT_FILE | awk '{print int($1)}'`
echo "Scale for $TITLE : $SCALE s"
RRA="RRA:MIN:0.5:$SCALE:3600 RRA:MAX:0.5:$SCALE:3600 RRA:AVERAGE:0.5:$SCALE:3600"
DS="DS:receiveOk:COUNTER:5:0:U DS:receiveError:COUNTER:5:0:U DS:transmitOk:COUNTER:5:0:U DS:transmitError:COUNTER:5:0:U"
TIME=1420070400
rrdtool create $RRD --step 1 $DS $RRA
echo RRD created
cat $INPUT_FILE | grep -v "^-" | awk "{print int(\$1) + $TIME \":\" \$2 \":\" \$3 \":\" \$4 \":\" \$5 \" \"}" | xargs rrdtool update $RRD
echo RRD filled
rrdtool graph $OUTPUT_FILE \
\
DEF:transmit0=$RRD:transmitOk:AVERAGE \
DEF:transmitError0=$RRD:transmitError:AVERAGE \
DEF:receive0=$RRD:receiveOk:AVERAGE \
DEF:receiveError0=$RRD:receiveError:AVERAGE \
\
AREA:transmit0`$BASE/../get_color.sh 1`:"Eth0 transmit      " \
GPRINT:transmit0:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:transmit0:MAX:"Maximum\:%8.2lf %s\n" \
CDEF:receive=receive0,-1,* \
AREA:receive`$BASE/../get_color.sh 2`:"Eth0 receive       " \
GPRINT:receive0:AVERAGE:"Average\:%8.2lf %s" \
GPRINT:receive0:MAX:"Maximum\:%8.2lf %s\n" \
LINE2:transmitError0`$BASE/../get_color.sh 3`:"Eth0 transmit error" \
GPRINT:transmitError0:MIN:"Minimum\:%8.2lf %s" \
GPRINT:transmitError0:MAX:"Maximum\:%8.2lf %s\n" \
LINE2:receiveError0`$BASE/../get_color.sh 4`:"Eth0 receive error " \
GPRINT:receiveError0:MIN:"Minimum\:%8.2lf %s" \
GPRINT:receiveError0:MAX:"Maximum\:%8.2lf %s\n" \
--slope-mode \
--title "$TITLE" \
--width 700 \
--height 300 \
--start $TIME \
--end start+$SIZE \
--x-grid SECOND:5:MINUTE:2:SECOND:30:0:%M:%S

