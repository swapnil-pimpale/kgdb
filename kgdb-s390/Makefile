CROSS_COMPILE	= s390-ibm-linux-
CC		= $(CROSS_COMPILE)gcc
KERNELDIR	= /home/ganesh/work/usr/src/linux-2.4.9-37
CFLAGS		= -D__KERNEL__ -I$(KERNELDIR)/include -Wall -Wstrict-prototypes -Wno-trigraphs \
			-O2 -fno-omit-frame-pointer -fno-strict-aliasing -fno-common \
			-Wno-unused -pipe -fno-strength-reduce -DMODULE -DEXPORT_SYMTAB

gdbmod.o:	gdbmod.c
	$(CC) $(CFLAGS) -c $<

clean:
	/bin/rm -f gdbmod.o
