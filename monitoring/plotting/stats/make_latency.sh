#!/bin/sh

BASE=`dirname $0`
INPUT_FILE=$1
HTML_DIR=$2
SCALE=$3

echo Processing latency file $INPUT_FILE to $HTML_DIR

HTML=$HTML_DIR/latency.html
echo "<h1>Latency</h1>" > $HTML

BASE=`dirname $0`

RRD=/tmp/tmp.rrd
SIZE=`tail -n 1 $INPUT_FILE | awk '{print int($1)}'`
DS=""
AWK_CMD=""
DEFL=""
DEFD=""
K=2
J=1

NODES=`cat nodes && echo "supervisor"`

for i in $NODES; do
	PADDED=`printf '%-18s' $i | perl -pe 's/ /@/g'`
	DS="$DS DS:$i-l:GAUGE:20:0:U "
	DEFL="$DEFL DEF:$i-l=$RRD:$i-l:AVERAGE"
	DEFL="$DEFL LINE2:$i-l`$BASE/../get_color.sh $J`:\"$PADDED\""
	DEFL="$DEFL GPRINT:$i-l:AVERAGE:\"Average\:%8.2lf %s\""
	DEFL="$DEFL GPRINT:$i-l:MAX:\"Maximum\:%8.2lf %s\""
	DEFL="$DEFL GPRINT:$i-l:MIN:\"Minimum\:%8.2lf %s\\\\n\""
	AWK_CMD="$AWK_CMD \":\" \$$K "
	K=`expr $K + 1`
	
	DS="$DS DS:$i-d:GAUGE:20:U:U "
	DEFD="$DEFD DEF:$i-d=$RRD:$i-d:AVERAGE"
	DEFD="$DEFD LINE2:$i-d`$BASE/../get_color.sh $J`:\"$PADDED\""
	DEFD="$DEFD GPRINT:$i-d:AVERAGE:\"Average\:%8.2lf %s\""
	DEFD="$DEFD GPRINT:$i-d:MAX:\"Maximum\:%8.2lf %s\""
	DEFD="$DEFD GPRINT:$i-d:MIN:\"Minimum\:%8.2lf %s\\\\n\""
	AWK_CMD="$AWK_CMD \":\" \$$K "
	K=`expr $K + 1`

	J=`expr $J + 1`
done

RRA="RRA:MIN:0.5:$SCALE:3600 RRA:MAX:0.5:$SCALE:3600 RRA:AVERAGE:0.5:$SCALE:3600"
rrdtool create $RRD --step 1 $DS $RRA
echo RRD created
TIME=1420070400
AWK_CMD="{print int(\$1) + $TIME $AWK_CMD}"
cat $INPUT_FILE | grep -v timestamp | awk "$AWK_CMD" | xargs rrdtool update $RRD
echo RRD filled

TITLE="Erlang latency (ms)"

echo $DEFL | perl -pe 's/@/ /g' | xargs rrdtool graph $HTML_DIR/latency.png \
--slope-mode \
--title "$TITLE" \
--width 700 \
--height 300 \
--start $TIME \
--end start+$SIZE \
--x-grid SECOND:5:MINUTE:2:SECOND:30:0:%M:%S \

echo "<img src='latency.png'/><br>" >> $HTML

TITLE="Delta time (ms)"
echo $DEFD | perl -pe 's/@/ /g' | xargs rrdtool graph $HTML_DIR/delta.png \
--slope-mode \
--title "$TITLE" \
--width 700 \
--height 300 \
--start $TIME \
--end start+$SIZE \
--x-grid SECOND:5:MINUTE:2:SECOND:30:0:%M:%S

echo "<img src='delta.png'/><br>" >> $HTML
