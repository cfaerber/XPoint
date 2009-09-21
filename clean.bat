@echo off
for %%a in (. netcall xplib objcom dos32 units) do for %%b in (dcu ppw ow owr ppu o) do del %%a\*.%%b 2>nul
