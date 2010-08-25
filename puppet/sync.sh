#~/bin/bash

for i in modules manifests;
do
    rsync -Pr --delete $i root@$1:/etc/puppet
done
