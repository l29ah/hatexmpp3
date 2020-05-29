#!/bin/bash
chatpath=test/mucs/smac@conference.bitcheese.net/__chat
dd if="$chatpath" iflag=nonblock bs=65536 | while read -r line; do
url=$(echo "$line" | sed -ne 's#.*\(http[^ ]*\).*#\1#p')
if [ -n "$url" ]; then
	(
	title=$(curl -Ls "$url" | sed -ne 's#.*<title>\([^<]*\)<.*#\1#p')
	echo -n "$title" >> "$chatpath"
	) &
fi
done
