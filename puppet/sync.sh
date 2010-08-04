#~/bin/bash

rsync -Pr --delete . root@$1:/etc/puppet/
