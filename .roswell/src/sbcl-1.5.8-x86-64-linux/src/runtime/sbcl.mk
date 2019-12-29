CC=gcc
LD=ld
CFLAGS=-g -Wall -Wundef -Wsign-compare -Wpointer-arith -O3 -D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -Wunused-parameter -fno-omit-frame-pointer -momit-leaf-frame-pointer -fno-pie
ASFLAGS=-g -Wall -Wundef -Wsign-compare -Wpointer-arith -O3 -D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -Wunused-parameter -fno-omit-frame-pointer -momit-leaf-frame-pointer -fno-pie
LINKFLAGS=-g -Wl,--export-dynamic -no-pie
LDFLAGS=-no-pie
__LDFLAGS__= -no-pie
LIBS=-ldl -lpthread -lz -lm
