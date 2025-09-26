#
# Regular cron jobs for the osvm package.
#
0 4	* * *	root	[ -x /usr/bin/osvm_maintenance ] && /usr/bin/osvm_maintenance
