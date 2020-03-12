#!/bin/bash
set -x
export sock=/tmp/alibot-socket
dir=alibot-dir
c="$1"
chatpath="$dir/mucs/$c/__chat"

9umount "$dir"
rm -f "$sock"
export HATEXMPP_ADDRESS="unix!$sock"
../dist/build/hatexmpp/hatexmpp &
while [ ! -e $sock ]; do sleep 1; done
mkdir -p "$dir"
9mount "$sock" "$dir"
trap 'rm "$sock"; 9umount "$dir"' EXIT
cp ~/.config/hatexmpp3/* "$dir/config/"
echo -n alibot > "$dir/config/resource"
echo -n alibot > "$dir/config/muc_default_nick"
mkdir "$dir/roster"
sleep 1
mkdir "$dir/mucs/$c"
sleep 1
dd if="$chatpath" iflag=nonblock bs=65536 | while read -r line; do
url=$(echo "$line" | sed -ne 's#.*http[^ ]*aliexpress.*/item/\([^ ?]*\)*.*#https://aliexpress.com/i/\1#p')
if [ -n "$url" ]; then
	echo -n "$url" >> "$chatpath"
fi
done
