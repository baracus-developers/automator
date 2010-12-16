# NETWORK

# netmask=255.255.255.0
# gateway=151.155.230.254
# dnsdomain=lsg.lab.novell.com
# dns1=155.155.230.1

# netmask=255.255.255.0
# gateway=192.168.1.1
ip=dhcp
dnsdomain=cloudbuilder.net
dns1=192.168.122.1

# define your modules here
module=puppet

#hardware definitions
hardware=default-vda

#distro
distro=sles-11.1-x86_64

#autobuild
autobuild=sumatra

# LOCALIZATION
#
#           SUSE                RHEL
timezone= "America/New York"  "America/New York" 
keymap=   english-us          us
lang=     en_US               en_US.UTF-8
langs=    "en_US de_DE"       # unused #

# UBUNTU SPECIFIC INSTALLER OPTIONS
#
# uncomment these for a profile to be used with Ubuntu

# consoleaskdetect=false
# consolelayoutcode=us


# DISTRO and HARDWARE, MODULE and ADDON lists
#
# these can be specified within this file vs cmdline
#
# NOTE:  that command line values will override these

# distro=sles-11.1-x86_64
# hardware=default
# addon="sles-11.1-ha-x86_64 sles-11.1-smt-x86_64"
# module="lsg_serial.sh psoagent2.1_vmhost_sle11 reboot"

