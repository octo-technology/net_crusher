#!/bin/sh

NODES=`cat nodes && echo "supervisor"`
BASE=`dirname $0`

STATS=`pwd`
HTML=$STATS/html
SCALE=$1

mkdir -p $HTML

HTML_NET=$HTML/net.html
HTML_CPU=$HTML/cpu.html
HTML_MEM=$HTML/mem.html
HTML_TCP=$HTML/tcp.html

cat <<EOT > $HTML/index.html
<h1>Results</h1>
<pre>
Scale for graph : $SCALE
</pre>
<script type="text/javascript">
function process(data) {
var lines = data.split("\n")
var table = document.createElement("table")
table.setAttribute("border", "1")
var tr0 = document.createElement('tr')

var th0 = document.createElement('th')
th0.innerHTML = "eventName"
tr0.appendChild(th0)

var th1 = document.createElement('th')
th1.innerHTML = "count"
tr0.appendChild(th1)

var th2 = document.createElement('th')
th2.innerHTML = "min"
tr0.appendChild(th2)

var th3 = document.createElement('th')
th3.innerHTML = "max"
tr0.appendChild(th3)

var th4 = document.createElement('th')
th4.innerHTML = "max - min"
tr0.appendChild(th4)

var th5 = document.createElement('th')
th5.innerHTML = "average"
tr0.appendChild(th5)

var th6 = document.createElement('th')
th6.innerHTML = "deviation"
tr0.appendChild(th6)

table.appendChild(tr0)
for (var i=0; i < lines.length; i++) {
        var tr = document.createElement('tr')
        var cells = lines[i].split(';')
        for (var j=0; j < cells.length; j++) {
                var td = document.createElement('td')
                td.innerHTML = cells[j]
                tr.appendChild(td)
        }
        table.appendChild(tr)
			}
			document.body.appendChild(table)
			}

			var http_request = new XMLHttpRequest();

			http_request.onreadystatechange = alertContents;
			http_request.open('GET', window.location + '../res.txt', true);
			http_request.send(null);

			function alertContents() {
			if (http_request.readyState == 4) {
			         if (http_request.status == 200) {
			        var data = http_request.responseText;
			process(data);
			}
			}
			}
			</script>
<a href='net.html'>Rezo stats</a><br/>
<a href='cpu.html'>CPU stats</a><br/>
<a href='mem.html'>Memory stats</a><br/>
<a href='latency.html'>Latency stats</a><br/>
<br/>
EOT


echo "<h1>Networks statistics</h1>" > $HTML_NET
echo "<h1>Cpu statistics</h1>" > $HTML_CPU
echo "<h1>Memory statistics</h1>" > $HTML_MEM
echo "<h1>Tcp statistics</h1>" > $HTML_TCP

$BASE/stats/make_latency.sh $STATS/latency.csv $HTML $SCALE

for i in $NODES; do
	echo Processing node $i
	PNG=$i\_net.png
	$BASE/stats/make_net.sh $i $STATS/$i\_net.csv $HTML/$PNG $SCALE
	echo "<img src='$PNG'/><br/>" >> $HTML_NET
	PNG=$i\_mem.png
	$BASE/stats/make_mem.sh $i $STATS/$i\_mem.csv $HTML/$PNG $SCALE
	echo "<img src='$PNG'/><br/>" >> $HTML_MEM
#	PNG=$i\_tcp.png
#        $BASE/stats/make_tcp.sh $i $STATS/$i\_tcp.csv $HTML/$PNG
#        echo "<img src='$PNG'/><br/>" >> $HTML_TCP
	PNG=$i\_cpu.png
        $BASE/stats/make_stat.sh $i $STATS/$i\_stat.csv $HTML/$PNG $SCALE
        echo "<img src='$PNG'/><br/>" >> $HTML_CPU
done
