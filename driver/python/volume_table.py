# volume_table[sample,volume] = (signed)(sample * volume) + bias (64/4ch)

import struct

output = open("volume_table.bin","wb")


#for volume in range(0,64):
#    for sample in range(0,32):
#        out = struct.pack("b", int(round(((sample) * volume) / 64)))
#        output.write( out )
#    for sample in range(-32,0):
#        out = struct.pack("b", int(round(((sample) * volume) / 64)))
#        output.write( out )

#for volume in range(0,64):
#    for sample in range(0,64):
#        out = struct.pack("b", int(round(((sample-32) * volume) / 64) + 16))
#        output.write( out )

for volume in range(0,32):
    for sample in range(0,64):
#        realvol = 2.0**(volume/31.0)-1
        realvol = volume/32.0
        if( volume == 31 ):
            realvol = 1.0
        value = round((sample-32.0) * realvol)
        out = struct.pack("b", int(value))
        output.write(out)


output.close()

