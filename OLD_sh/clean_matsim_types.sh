#!/bin/bash

# remove leading whitespace from each line
sed 's|^[[:blank:]]*||g' < data/wfrc_network/highway_network.xml > tl1.xml
# remove beginning of attributes tag
perl -i.bak1 -p -e 's/<attributes>\n//g;' tl1.xml
# remove end of attributes tag
perl -i.bak2 -p -e 's/<\/attributes>\n//g;' tl1.xml
# remove end of link tag
perl -i.bak3 -p -e 's/<\/link>\n//g;' tl1.xml
# remove
perl -i.bak4 -p -e 's/<attribute name="type" class="java.lang.String">/type="/g;' tl1.xml
perl -i.bak5 -p -e 's/<\/attribute>/"\/\>/g;' tl1.xml
perl -i.bak4 -p -e 's/modes="car" >\n/modes="car" /g;' tl1.xml

mv tl1.xml data/wfrc_network/highway_network_for_beam.xml
rm -rf tl1.*
