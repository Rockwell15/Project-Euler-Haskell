import os

for name in os.listdir("."):
  if name.endswith(".hs"):
    os.rename( name, name.zfill(6) )
