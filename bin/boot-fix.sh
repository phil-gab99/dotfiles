# Run ls (hd0) or (hd1) and select the appropriate partitions until /boot/grub/ is found
# Then apply the following

set prefix=(hd1,1)/boot/grub/
set root=(hd0,1)
insmod linux
insmod normal
normal

# It should boot normally afterwards
# Find out how to run sudo update-grub afterwards
