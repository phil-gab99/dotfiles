# Run ls (hd0) or (hd1) and select the appropriate partitions until /boot/grub/ is found
# Then apply the following (Below is the appropriate values for this machine)

set prefix=(hd1,3)/boot/grub/
set root=(hd1,3)
insmod linux
insmod normal
normal

# It should boot normally afterwards
# Find out how to run sudo update-grub afterwards
