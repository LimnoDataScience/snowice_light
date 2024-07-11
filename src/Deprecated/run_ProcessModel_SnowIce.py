import numpy as np
import pandas as pd
import os
from math import pi, exp, sqrt
from scipy.interpolate import interp1d
from copy import deepcopy
import datetime
import matplotlib.pyplot as plt
import seaborn as sns
from numba import jit
import matplotlib

#os.chdir("/home/robert/Projects/LakePIAB/src")
os.chdir("C:/Users/ladwi/Documents/Projects/R/snowice_light/src")
#from oneD_HeatMixing_Functions import get_hyp()sography, provide_meteorology, initial_profile, run_thermalmodel_v1, run_hybridmodel_heating, run_hybridmodel_mixing, run_thermalmodel_v2
from processBased_lakeModel_functions import get_hypsography, provide_meteorology, initial_profile, run_thermalmodel, run_thermalmodel_specific, run_thermalmodel_test #, heating_module, diffusion_module, mixing_module, convection_module, ice_module


## lake configurations
zmax = 5 # maximum lake depth
nx = 5 * 2 # number of layers we will have
dt = 3600 # 24 hours times 60 min/hour times 60 seconds/min
dx = zmax/nx # spatial step

## area and depth values of our lake 
hyps_all = get_hypsography(hypsofile = '../input/bathymetry.csv',
                            dx = dx, nx = nx)
                            
## atmospheric boundary conditions
meteo_all = provide_meteorology(meteofile = '../input/Mendota_2002.csv',
                    secchifile = None, 
                    windfactor = 1.0)
                     
# hydrodynamic_timestep = 24 * dt
# total_runtime =  365 *1 # 14 * 365
# startTime = 365*10#150 * 24 * 3600
# endTime =  (startTime + total_runtime * hydrodynamic_timestep) - 1

# startingDate = meteo_all[0]['date'][startTime* hydrodynamic_timestep/dt]
# endingDate = meteo_all[0]['date'][(startTime + total_runtime) * hydrodynamic_timestep/dt -1]
# # endingDate = meteo_all[0]['date'][(startTime + total_runtime * hydrodynamic_timestep/dt) - 1]

# #26280
# times = pd.date_range(startingDate, endingDate, freq='H')

# nTotalSteps = int(total_runtime * hydrodynamic_timestep/ dt)

hydrodynamic_timestep = 24 * dt
total_runtime =  (200) * hydrodynamic_timestep/dt  #365 *1 # 14 * 365
startTime =   (0 + 300*12) * hydrodynamic_timestep/dt #150 * 24 * 3600
endTime =  (startTime + total_runtime)  # * hydrodynamic_timestep/dt) - 1

startingDate = meteo_all[0]['date'][startTime] #* hydrodynamic_timestep/dt]
endingDate = meteo_all[0]['date'][(endTime-1)]#meteo_all[0]['date'][(startTime + total_runtime)]# * hydrodynamic_timestep/dt -1]
# endingDate = meteo_all[0]['date'][(startTime + total_runtime * hydrodynamic_timestep/dt) - 1]


#26280
times = pd.date_range(startingDate, endingDate, freq='H')

nTotalSteps = int(total_runtime) #  * hydrodynamic_timestep/ dt)

## here we define our initial profile
u_ini = initial_profile(initfile = '../input/observedTemp.txt', nx = nx, dx = dx,
                     depth = hyps_all[1],
                     startDate = startingDate)

Start = datetime.datetime.now()


light_extinction = 1.5
pp_factor = 2
meteo_all[0]["Precipitation_millimeterPerDay"] = meteo_all[0]["Precipitation_millimeterPerDay"] * pp_factor# * 0.0
    
res = run_thermalmodel_test(  
    u = deepcopy(u_ini),
    startTime = startTime, 
    endTime = endTime, #( startTime + total_runtime * hydrodynamic_timestep) - 1,
    area = hyps_all[0][:-1],
    volume = hyps_all[2][:-1],
    depth = hyps_all[1][:-1],
    zmax = zmax,
    nx = nx,
    dt = dt,
    dx = dx,
    daily_meteo = meteo_all[0],
    secview = meteo_all[1],
    ice = False,
    Hi = 0,
    Hs = 0,
    Hsi = 0,
    iceT = 6,
    supercooled = 0,
    diffusion_method = 'hendersonSellers',# 'hendersonSellers', 'munkAnderson' 'hondzoStefan'
    scheme='implicit',
    km = 1.4 * 10**(-7), # 4 * 10**(-6), 
    weight_kz = 0.5,
    kd_light = light_extinction, 
    denThresh=1e-3,
    albedo = 0.1,
    eps=0.97,
    emissivity=0.97,
    sigma=5.67e-8,
    sw_factor = 1.0,
    wind_factor = 1.0,
    p2=1,
    B=0.61,
    g=9.81,
    Cd = 0.0013, # momentum coeff (wind)
    meltP=1,
    dt_iceon_avg=0.8,
    Hgeo=0.1, # geothermal heat 
    KEice=0,
    Ice_min=0.1,
    pgdl_mode = 'on',
    rho_snow = 250,
    iceTemp = 0)

temp=  res['temp']
diff =  res['diff']
avgtemp = res['average'].values
temp_initial =  res['temp_initial']
temp_heat=  res['temp_heat']
temp_diff=  res['temp_diff']
temp_mix =  res['temp_mix']
temp_conv =  res['temp_conv']
temp_ice=  res['temp_ice']
meteo=  res['meteo_input']
buoyancy = res['buoyancy']
icethickness= res['icethickness']
snowthickness= res['snowthickness']
snowicethickness= res['snowicethickness']



# convert averages from array to data frame
avgtemp_df = pd.DataFrame(avgtemp, columns=["time", "thermoclineDep", "epi", "hypo", "tot", "stratFlag"])
avgtemp_df.insert(2, "icethickness", icethickness[0,], True)
avgtemp_df.insert(2, "snowthickness", snowthickness[0,], True)
avgtemp_df.insert(2, "snowicethickness", snowicethickness[0,], True)

End = datetime.datetime.now()
print(End - Start)

    
# epi/hypo/total
colors = ['#F8766D', '#00BA38', '#619CFF']
avgtemp_df.plot(x='time', y=['epi', 'hypo', 'tot'], color=colors, kind='line')
plt.show()

# stratflag
avgtemp_df.plot(x='time', y=['stratFlag'], kind='line', color="black")
plt.show()

# thermocline depth
avgtemp_df.plot(x='time', y=['thermoclineDep'], color="black")
plt.gca().invert_yaxis()
plt.scatter(avgtemp_df.time, avgtemp_df.stratFlag, c=avgtemp_df.stratFlag)
plt.show()

# ice thickness
avgtemp_df.plot(x='time', y=['icethickness'], color="black")
plt.show()

# snowice thickness
avgtemp_df.plot(x='time', y=['snowicethickness'], color="black")
plt.show()

# snow thickness
avgtemp_df.plot(x='time', y=['snowthickness'], color="black")
plt.show()


fig=plt.figure(figsize=(10,6))
plt.plot(times, avgtemp_df["icethickness"], color="black")
plt.plot(times, avgtemp_df["snowicethickness"], color="blue")
plt.plot(times, avgtemp_df["snowthickness"], color="cyan")
plt.legend(['Ice', 'Snowice', 'Snow'])
plt.title('Kd ' + str(light_extinction) + ", PP " + str(pp_factor))
plt.ylabel('Thickness (m)')
plt.show()

fig=plt.figure(figsize=(10,6))
ax = plt.gca()
plt.plot(times, temp[1,:], color="blue")
plt.plot(times, temp[2,:], color="red")
plt.plot(times, temp[3,:], color="green")
plt.legend(['0.5 m', '1.0 m', '1.5 m'])
plt.title('Kd ' + str(light_extinction) + ", PP " + str(pp_factor))
plt.ylabel('Water Temperature  ($^\circ$C)')
ax.set_ylim([-0.1, 3])
plt.show()

plt.plot(meteo[16,:])
plt.show()

# heatmap of temps  
N_pts = 6

## function to calculate density from temperature

def calc_dens(wtemp):
    dens = (999.842594 + (6.793952 * 1e-2 * wtemp) - (9.095290 * 1e-3 *wtemp**2) +
      (1.001685 * 1e-4 * wtemp**3) - (1.120083 * 1e-6* wtemp**4) + 
      (6.536336 * 1e-9 * wtemp**5))
    return dens

fig, ax = plt.subplots(figsize=(17,7))
sns.heatmap(temp, cmap=plt.cm.get_cmap('Spectral_r'),  xticklabels=1000, yticklabels=2, vmin = 0, vmax = 15)
ax.contour(np.arange(.5, temp.shape[1]), np.arange(.5, temp.shape[0]), calc_dens(temp), levels=[999.9],
           colors=['black', 'gray'],
           linestyles = 'dotted')
ax.set_ylabel("Depth (m)", fontsize=15)
ax.set_xlabel("Time", fontsize=15)    
ax.collections[0].colorbar.set_label("Water Temperature  ($^\circ$C)")
xticks_ix = np.array(ax.get_xticks()).astype(int)
time_label = times[xticks_ix]
nelement = len(times)//N_pts
ax.set_xticklabels(time_label, rotation=0)
yticks_ix = np.array(ax.get_yticks()).astype(int)
depth_label = yticks_ix / 2
ax.set_yticklabels(depth_label, rotation=0)
plt.show()
