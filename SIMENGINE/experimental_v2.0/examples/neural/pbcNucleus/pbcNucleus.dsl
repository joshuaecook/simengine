import "canPBC.dsl"
import "synapse.dsl"

model (Vm1, Vm2, Vm3, Vm4, Vm5, Vm6, Vm7, Vm8, Vm9, Vm10, Vm11, Vm12, Vm13, Vm14, Vm15, Vm16, Vm17, Vm18, Vm19, Vm20, Vm21, Vm22, Vm23, Vm24, Vm25, Vm26, Vm27, Vm28, Vm29, Vm30, Vm31, Vm32, Vm33, Vm34, Vm35, Vm36, Vm37, Vm38, Vm39, Vm40, Vm41, Vm42, Vm43, Vm44, Vm45, Vm46, Vm47, Vm48, Vm49, Vm50, Vm51, Vm52, Vm53, Vm54, Vm55, Vm56, Vm57, Vm58, Vm59, Vm60, Vm61, Vm62, Vm63, Vm64, Vm65, Vm66, Vm67, Vm68, Vm69, Vm70, Vm71, Vm72, Vm73, Vm74, Vm75, Vm76, Vm77, Vm78, Vm79, Vm80, Vm81, Vm82, Vm83, Vm84, Vm85, Vm86, Vm87, Vm88, Vm89, Vm90, Vm91, Vm92, Vm93, Vm94, Vm95, Vm96, Vm97, Vm98, Vm99, Vm100, Vm101, Vm102, Vm103, Vm104, Vm105, Vm106, Vm107, Vm108, Vm109, Vm110, Vm111, Vm112, Vm113, Vm114, Vm115, Vm116, Vm117, Vm118, Vm119, Vm120, Vm121, Vm122, Vm123, Vm124, Vm125, Vm126, Vm127, Vm128) = pbcNucleus(IStim)
input IStim with {default = 0}

println "Starting submodel instantiations"

submodel canPBC pbc94 with {gnap = 1.7, gcan = 0}
submodel canPBC pbc87 with {gnap = 1.7, gcan = 0.1}
submodel canPBC pbc3 with {gnap = 1.75, gcan = 0}
submodel canPBC pbc114 with {gnap = 1.7, gcan = 0}
submodel canPBC pbc68 with {gnap = 1.65, gcan = 0}
submodel canPBC pbc40 with {gnap = 1.65, gcan = 0.05}
submodel canPBC pbc21 with {gnap = 1.65, gcan = 0.15}
submodel canPBC pbc17 with {gnap = 1.55, gcan = 0}
submodel canPBC pbc51 with {gnap = 1.6, gcan = 0.05}
submodel canPBC pbc9 with {gnap = 1.55, gcan = 0}
submodel canPBC pbc46 with {gnap = 1.75, gcan = 0}
submodel canPBC pbc54 with {gnap = 1.6, gcan = 0.05}
submodel canPBC pbc25 with {gnap = 1.65, gcan = 0.1}
submodel canPBC pbc39 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc85 with {gnap = 1.7, gcan = 0}
submodel canPBC pbc74 with {gnap = 1.7, gcan = 0}
submodel canPBC pbc75 with {gnap = 1.75, gcan = 0}
submodel canPBC pbc79 with {gnap = 1.65, gcan = 0.2}
submodel canPBC pbc35 with {gnap = 1.65, gcan = 0.05}
submodel canPBC pbc81 with {gnap = 1.6, gcan = 0.05}
submodel canPBC pbc84 with {gnap = 1.75, gcan = 0}
submodel canPBC pbc109 with {gnap = 1.7, gcan = 0}
submodel canPBC pbc1 with {gnap = 1.65, gcan = 0.2}
submodel canPBC pbc124 with {gnap = 1.65, gcan = 0.2}
submodel canPBC pbc59 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc32 with {gnap = 1.65, gcan = 0}
submodel canPBC pbc18 with {gnap = 1.65, gcan = 0}
submodel canPBC pbc72 with {gnap = 1.65, gcan = 0.1}
submodel canPBC pbc42 with {gnap = 1.65, gcan = 0.15}
submodel canPBC pbc111 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc92 with {gnap = 1.55, gcan = 0}
submodel canPBC pbc52 with {gnap = 1.75, gcan = 0}
submodel canPBC pbc34 with {gnap = 1.65, gcan = 0.1}
submodel canPBC pbc30 with {gnap = 1.7, gcan = 0.15}
submodel canPBC pbc62 with {gnap = 1.75, gcan = 0}
submodel canPBC pbc45 with {gnap = 1.55, gcan = 0}
submodel canPBC pbc37 with {gnap = 1.65, gcan = 0.1}
submodel canPBC pbc119 with {gnap = 1.6, gcan = 0.05}
submodel canPBC pbc118 with {gnap = 1.7, gcan = 0.15}
submodel canPBC pbc6 with {gnap = 1.55, gcan = 0}
submodel canPBC pbc65 with {gnap = 1.7, gcan = 0.1}
submodel canPBC pbc97 with {gnap = 1.65, gcan = 0.15}
submodel canPBC pbc113 with {gnap = 1.7, gcan = 0}
submodel canPBC pbc33 with {gnap = 1.65, gcan = 0}
submodel canPBC pbc57 with {gnap = 1.7, gcan = 0.15}
submodel canPBC pbc80 with {gnap = 1.65, gcan = 0.15}
submodel canPBC pbc60 with {gnap = 1.65, gcan = 0.2}
submodel canPBC pbc91 with {gnap = 1.65, gcan = 0.1}
submodel canPBC pbc14 with {gnap = 1.65, gcan = 0.05}
submodel canPBC pbc22 with {gnap = 1.65, gcan = 0}
submodel canPBC pbc15 with {gnap = 1.7, gcan = 0}
submodel canPBC pbc67 with {gnap = 1.7, gcan = 0.1}
submodel canPBC pbc64 with {gnap = 1.7, gcan = 0.1}
submodel canPBC pbc108 with {gnap = 1.65, gcan = 0.05}
submodel canPBC pbc69 with {gnap = 1.7, gcan = 0.1}
submodel canPBC pbc29 with {gnap = 1.55, gcan = 0}
submodel canPBC pbc4 with {gnap = 1.65, gcan = 0.2}
submodel canPBC pbc82 with {gnap = 1.6, gcan = 0.05}
submodel canPBC pbc71 with {gnap = 1.65, gcan = 0}
submodel canPBC pbc107 with {gnap = 1.7, gcan = 0.1}
submodel canPBC pbc93 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc13 with {gnap = 1.7, gcan = 0.1}
submodel canPBC pbc63 with {gnap = 1.6, gcan = 0.05}
submodel canPBC pbc55 with {gnap = 1.75, gcan = 0}
submodel canPBC pbc112 with {gnap = 1.65, gcan = 0.15}
submodel canPBC pbc95 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc73 with {gnap = 1.65, gcan = 0.05}
submodel canPBC pbc24 with {gnap = 1.7, gcan = 0.15}
submodel canPBC pbc58 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc61 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc122 with {gnap = 1.65, gcan = 0}
submodel canPBC pbc90 with {gnap = 1.7, gcan = 0}
submodel canPBC pbc89 with {gnap = 1.7, gcan = 0.1}
submodel canPBC pbc115 with {gnap = 1.7, gcan = 0.15}
submodel canPBC pbc44 with {gnap = 1.65, gcan = 0.2}
submodel canPBC pbc86 with {gnap = 1.7, gcan = 0.15}
submodel canPBC pbc43 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc128 with {gnap = 1.65, gcan = 0}
submodel canPBC pbc106 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc49 with {gnap = 1.75, gcan = 0}
submodel canPBC pbc41 with {gnap = 1.65, gcan = 0.2}
submodel canPBC pbc105 with {gnap = 1.7, gcan = 0.15}
submodel canPBC pbc11 with {gnap = 1.7, gcan = 0}
submodel canPBC pbc76 with {gnap = 1.7, gcan = 0.1}
submodel canPBC pbc88 with {gnap = 1.55, gcan = 0}
submodel canPBC pbc47 with {gnap = 1.7, gcan = 0.1}
submodel canPBC pbc125 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc100 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc126 with {gnap = 1.7, gcan = 0.15}
submodel canPBC pbc70 with {gnap = 1.55, gcan = 0}
submodel canPBC pbc5 with {gnap = 1.7, gcan = 0.1}
submodel canPBC pbc99 with {gnap = 1.65, gcan = 0.05}
submodel canPBC pbc98 with {gnap = 1.65, gcan = 0.15}
submodel canPBC pbc127 with {gnap = 1.65, gcan = 0}
submodel canPBC pbc66 with {gnap = 1.6, gcan = 0.05}
submodel canPBC pbc12 with {gnap = 1.65, gcan = 0.15}
submodel canPBC pbc20 with {gnap = 1.7, gcan = 0.15}
submodel canPBC pbc28 with {gnap = 1.75, gcan = 0}
submodel canPBC pbc38 with {gnap = 1.65, gcan = 0.05}
submodel canPBC pbc16 with {gnap = 1.65, gcan = 0.05}
submodel canPBC pbc83 with {gnap = 1.7, gcan = 0.1}
submodel canPBC pbc26 with {gnap = 1.65, gcan = 0.2}
submodel canPBC pbc23 with {gnap = 1.6, gcan = 0.05}
submodel canPBC pbc56 with {gnap = 1.65, gcan = 0.15}
submodel canPBC pbc116 with {gnap = 1.7, gcan = 0.1}
submodel canPBC pbc120 with {gnap = 1.65, gcan = 0.15}
submodel canPBC pbc96 with {gnap = 1.7, gcan = 0.1}
submodel canPBC pbc7 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc104 with {gnap = 1.7, gcan = 0}
submodel canPBC pbc27 with {gnap = 1.55, gcan = 0}
submodel canPBC pbc101 with {gnap = 1.65, gcan = 0.15}
submodel canPBC pbc117 with {gnap = 1.65, gcan = 0.1}
submodel canPBC pbc102 with {gnap = 1.65, gcan = 0.2}
submodel canPBC pbc31 with {gnap = 1.7, gcan = 0.15}
submodel canPBC pbc123 with {gnap = 1.65, gcan = 0.2}
submodel canPBC pbc121 with {gnap = 1.65, gcan = 0.2}
submodel canPBC pbc2 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc19 with {gnap = 1.65, gcan = 0.05}
submodel canPBC pbc110 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc36 with {gnap = 1.65, gcan = 0.2}
submodel canPBC pbc50 with {gnap = 1.7, gcan = 0}
submodel canPBC pbc10 with {gnap = 1.65, gcan = 0.2}
submodel canPBC pbc77 with {gnap = 1.7, gcan = 0.25}
submodel canPBC pbc48 with {gnap = 1.65, gcan = 0.1}
submodel canPBC pbc53 with {gnap = 1.55, gcan = 0}
submodel canPBC pbc103 with {gnap = 1.65, gcan = 0.05}
submodel canPBC pbc8 with {gnap = 1.65, gcan = 0.1}
submodel canPBC pbc78 with {gnap = 1.75, gcan = 0}

println "Starting synapse instantiations"

submodel synapse syn1 with {Vpost = pbc1.Vm}
submodel synapse syn2 with {Vpost = pbc2.Vm}
submodel synapse syn3 with {Vpost = pbc3.Vm}
submodel synapse syn4 with {Vpost = pbc4.Vm}
submodel synapse syn5 with {Vpost = pbc5.Vm}
submodel synapse syn6 with {Vpost = pbc6.Vm}
submodel synapse syn7 with {Vpost = pbc7.Vm}
submodel synapse syn8 with {Vpost = pbc8.Vm}
submodel synapse syn9 with {Vpost = pbc9.Vm}
submodel synapse syn10 with {Vpost = pbc10.Vm}
submodel synapse syn11 with {Vpost = pbc11.Vm}
submodel synapse syn12 with {Vpost = pbc12.Vm}
submodel synapse syn13 with {Vpost = pbc13.Vm}
submodel synapse syn14 with {Vpost = pbc14.Vm}
submodel synapse syn15 with {Vpost = pbc15.Vm}
submodel synapse syn16 with {Vpost = pbc16.Vm}
submodel synapse syn17 with {Vpost = pbc17.Vm}
submodel synapse syn18 with {Vpost = pbc18.Vm}
submodel synapse syn19 with {Vpost = pbc19.Vm}
submodel synapse syn20 with {Vpost = pbc20.Vm}
submodel synapse syn21 with {Vpost = pbc21.Vm}
submodel synapse syn22 with {Vpost = pbc22.Vm}
submodel synapse syn23 with {Vpost = pbc23.Vm}
submodel synapse syn24 with {Vpost = pbc24.Vm}
submodel synapse syn25 with {Vpost = pbc25.Vm}
submodel synapse syn26 with {Vpost = pbc26.Vm}
submodel synapse syn27 with {Vpost = pbc27.Vm}
submodel synapse syn28 with {Vpost = pbc28.Vm}
submodel synapse syn29 with {Vpost = pbc29.Vm}
submodel synapse syn30 with {Vpost = pbc30.Vm}
submodel synapse syn31 with {Vpost = pbc31.Vm}
submodel synapse syn32 with {Vpost = pbc32.Vm}
submodel synapse syn33 with {Vpost = pbc33.Vm}
submodel synapse syn34 with {Vpost = pbc34.Vm}
submodel synapse syn35 with {Vpost = pbc35.Vm}
submodel synapse syn36 with {Vpost = pbc36.Vm}
submodel synapse syn37 with {Vpost = pbc37.Vm}
submodel synapse syn38 with {Vpost = pbc38.Vm}
submodel synapse syn39 with {Vpost = pbc39.Vm}
submodel synapse syn40 with {Vpost = pbc40.Vm}
submodel synapse syn41 with {Vpost = pbc41.Vm}
submodel synapse syn42 with {Vpost = pbc42.Vm}
submodel synapse syn43 with {Vpost = pbc43.Vm}
submodel synapse syn44 with {Vpost = pbc44.Vm}
submodel synapse syn45 with {Vpost = pbc45.Vm}
submodel synapse syn46 with {Vpost = pbc46.Vm}
submodel synapse syn47 with {Vpost = pbc47.Vm}
submodel synapse syn48 with {Vpost = pbc48.Vm}
submodel synapse syn49 with {Vpost = pbc49.Vm}
submodel synapse syn50 with {Vpost = pbc50.Vm}
submodel synapse syn51 with {Vpost = pbc51.Vm}
submodel synapse syn52 with {Vpost = pbc52.Vm}
submodel synapse syn53 with {Vpost = pbc53.Vm}
submodel synapse syn54 with {Vpost = pbc54.Vm}
submodel synapse syn55 with {Vpost = pbc55.Vm}
submodel synapse syn56 with {Vpost = pbc56.Vm}
submodel synapse syn57 with {Vpost = pbc57.Vm}
submodel synapse syn58 with {Vpost = pbc58.Vm}
submodel synapse syn59 with {Vpost = pbc59.Vm}
submodel synapse syn60 with {Vpost = pbc60.Vm}
submodel synapse syn61 with {Vpost = pbc61.Vm}
submodel synapse syn62 with {Vpost = pbc62.Vm}
submodel synapse syn63 with {Vpost = pbc63.Vm}
submodel synapse syn64 with {Vpost = pbc64.Vm}
submodel synapse syn65 with {Vpost = pbc65.Vm}
submodel synapse syn66 with {Vpost = pbc66.Vm}
submodel synapse syn67 with {Vpost = pbc67.Vm}
submodel synapse syn68 with {Vpost = pbc68.Vm}
submodel synapse syn69 with {Vpost = pbc69.Vm}
submodel synapse syn70 with {Vpost = pbc70.Vm}
submodel synapse syn71 with {Vpost = pbc71.Vm}
submodel synapse syn72 with {Vpost = pbc72.Vm}
submodel synapse syn73 with {Vpost = pbc73.Vm}
submodel synapse syn74 with {Vpost = pbc74.Vm}
submodel synapse syn75 with {Vpost = pbc75.Vm}
submodel synapse syn76 with {Vpost = pbc76.Vm}
submodel synapse syn77 with {Vpost = pbc77.Vm}
submodel synapse syn78 with {Vpost = pbc78.Vm}
submodel synapse syn79 with {Vpost = pbc79.Vm}
submodel synapse syn80 with {Vpost = pbc80.Vm}
submodel synapse syn81 with {Vpost = pbc81.Vm}
submodel synapse syn82 with {Vpost = pbc82.Vm}
submodel synapse syn83 with {Vpost = pbc83.Vm}
submodel synapse syn84 with {Vpost = pbc84.Vm}
submodel synapse syn85 with {Vpost = pbc85.Vm}
submodel synapse syn86 with {Vpost = pbc86.Vm}
submodel synapse syn87 with {Vpost = pbc87.Vm}
submodel synapse syn88 with {Vpost = pbc88.Vm}
submodel synapse syn89 with {Vpost = pbc89.Vm}
submodel synapse syn90 with {Vpost = pbc90.Vm}
submodel synapse syn91 with {Vpost = pbc91.Vm}
submodel synapse syn92 with {Vpost = pbc92.Vm}
submodel synapse syn93 with {Vpost = pbc93.Vm}
submodel synapse syn94 with {Vpost = pbc94.Vm}
submodel synapse syn95 with {Vpost = pbc95.Vm}
submodel synapse syn96 with {Vpost = pbc96.Vm}
submodel synapse syn97 with {Vpost = pbc97.Vm}
submodel synapse syn98 with {Vpost = pbc98.Vm}
submodel synapse syn99 with {Vpost = pbc99.Vm}
submodel synapse syn100 with {Vpost = pbc100.Vm}
submodel synapse syn101 with {Vpost = pbc101.Vm}
submodel synapse syn102 with {Vpost = pbc102.Vm}
submodel synapse syn103 with {Vpost = pbc103.Vm}
submodel synapse syn104 with {Vpost = pbc104.Vm}
submodel synapse syn105 with {Vpost = pbc105.Vm}
submodel synapse syn106 with {Vpost = pbc106.Vm}
submodel synapse syn107 with {Vpost = pbc107.Vm}
submodel synapse syn108 with {Vpost = pbc108.Vm}
submodel synapse syn109 with {Vpost = pbc109.Vm}
submodel synapse syn110 with {Vpost = pbc110.Vm}
submodel synapse syn111 with {Vpost = pbc111.Vm}
submodel synapse syn112 with {Vpost = pbc112.Vm}
submodel synapse syn113 with {Vpost = pbc113.Vm}
submodel synapse syn114 with {Vpost = pbc114.Vm}
submodel synapse syn115 with {Vpost = pbc115.Vm}
submodel synapse syn116 with {Vpost = pbc116.Vm}
submodel synapse syn117 with {Vpost = pbc117.Vm}
submodel synapse syn118 with {Vpost = pbc118.Vm}
submodel synapse syn119 with {Vpost = pbc119.Vm}
submodel synapse syn120 with {Vpost = pbc120.Vm}
submodel synapse syn121 with {Vpost = pbc121.Vm}
submodel synapse syn122 with {Vpost = pbc122.Vm}
submodel synapse syn123 with {Vpost = pbc123.Vm}
submodel synapse syn124 with {Vpost = pbc124.Vm}
submodel synapse syn125 with {Vpost = pbc125.Vm}
submodel synapse syn126 with {Vpost = pbc126.Vm}
submodel synapse syn127 with {Vpost = pbc127.Vm}
submodel synapse syn128 with {Vpost = pbc128.Vm}

println "Assigning inputs to synapses and neurons"

syn1.Spre = pbc4.s + pbc17.s + pbc40.s + pbc43.s + pbc55.s + pbc57.s + pbc59.s + pbc61.s + pbc62.s + pbc70.s + pbc82.s + pbc92.s + pbc100.s + pbc103.s + pbc119.s 
pbc1.Isyn = syn1.isynapse
syn2.Spre = pbc33.s + pbc50.s + pbc61.s + pbc74.s + pbc83.s + pbc108.s + pbc109.s + pbc113.s + pbc115.s 
pbc2.Isyn = syn2.isynapse
syn3.Spre = pbc7.s + pbc12.s + pbc23.s + pbc28.s + pbc76.s + pbc83.s + pbc84.s 
pbc3.Isyn = syn3.isynapse
syn4.Spre = pbc8.s + pbc30.s + pbc38.s + pbc66.s + pbc105.s + pbc107.s 
pbc4.Isyn = syn4.isynapse
syn5.Spre = pbc7.s + pbc9.s + pbc10.s + pbc12.s + pbc20.s + pbc54.s + pbc61.s + pbc62.s + pbc69.s + pbc71.s + pbc79.s + pbc88.s + pbc99.s + pbc100.s 
pbc5.Isyn = syn5.isynapse
syn6.Spre = pbc45.s + pbc62.s + pbc70.s + pbc84.s + pbc89.s + pbc95.s + pbc107.s 
pbc6.Isyn = syn6.isynapse
syn7.Spre = pbc15.s + pbc32.s + pbc48.s + pbc49.s + pbc55.s + pbc80.s + pbc96.s + pbc98.s + pbc128.s 
pbc7.Isyn = syn7.isynapse
syn8.Spre = pbc26.s + pbc41.s + pbc67.s + pbc88.s + pbc115.s + pbc119.s 
pbc8.Isyn = syn8.isynapse
syn9.Spre = pbc25.s + pbc26.s + pbc42.s + pbc60.s + pbc75.s + pbc80.s + pbc86.s + pbc88.s + pbc89.s + pbc101.s + pbc106.s + pbc118.s 
pbc9.Isyn = syn9.isynapse
syn10.Spre = pbc21.s + pbc23.s + pbc25.s + pbc30.s + pbc95.s + pbc113.s + pbc126.s 
pbc10.Isyn = syn10.isynapse
syn11.Spre = pbc2.s + pbc16.s + pbc21.s + pbc44.s + pbc60.s + pbc71.s + pbc85.s + pbc88.s + pbc90.s + pbc117.s 
pbc11.Isyn = syn11.isynapse
syn12.Spre = pbc44.s + pbc46.s + pbc54.s + pbc79.s + pbc90.s + pbc112.s + pbc117.s 
pbc12.Isyn = syn12.isynapse
syn13.Spre = pbc6.s + pbc11.s + pbc20.s + pbc21.s + pbc22.s + pbc24.s + pbc30.s + pbc32.s + pbc44.s + pbc48.s + pbc67.s + pbc70.s + pbc95.s + pbc100.s + pbc103.s + pbc107.s + pbc108.s + pbc110.s + pbc113.s + pbc116.s + pbc119.s + pbc124.s 
pbc13.Isyn = syn13.isynapse
syn14.Spre = pbc41.s + pbc46.s + pbc51.s + pbc74.s + pbc107.s + pbc109.s 
pbc14.Isyn = syn14.isynapse
syn15.Spre = pbc7.s + pbc30.s + pbc31.s + pbc46.s + pbc90.s + pbc93.s + pbc99.s 
pbc15.Isyn = syn15.isynapse
syn16.Spre = pbc7.s + pbc25.s + pbc43.s + pbc72.s + pbc73.s + pbc75.s + pbc96.s + pbc114.s + pbc115.s + pbc128.s 
pbc16.Isyn = syn16.isynapse
syn17.Spre = pbc5.s + pbc25.s + pbc39.s + pbc42.s + pbc66.s + pbc75.s + pbc98.s 
pbc17.Isyn = syn17.isynapse
syn18.Spre = pbc2.s + pbc22.s + pbc29.s + pbc65.s + pbc69.s + pbc78.s + pbc92.s + pbc120.s + pbc127.s 
pbc18.Isyn = syn18.isynapse
syn19.Spre = pbc23.s + pbc61.s + pbc70.s + pbc74.s + pbc77.s + pbc94.s + pbc120.s + pbc125.s 
pbc19.Isyn = syn19.isynapse
syn20.Spre = pbc9.s + pbc28.s + pbc34.s + pbc59.s + pbc63.s + pbc68.s + pbc73.s + pbc78.s + pbc88.s + pbc103.s + pbc106.s + pbc108.s + pbc113.s 
pbc20.Isyn = syn20.isynapse
syn21.Spre = pbc10.s + pbc11.s + pbc36.s + pbc51.s + pbc55.s + pbc58.s + pbc74.s + pbc80.s + pbc85.s + pbc87.s + pbc90.s + pbc98.s + pbc118.s 
pbc21.Isyn = syn21.isynapse
syn22.Spre = pbc31.s + pbc33.s + pbc35.s + pbc41.s + pbc67.s + pbc87.s + pbc95.s + pbc101.s + pbc117.s + pbc124.s + pbc125.s 
pbc22.Isyn = syn22.isynapse
syn23.Spre = pbc12.s + pbc33.s + pbc35.s + pbc68.s + pbc93.s + pbc106.s + pbc114.s + pbc126.s 
pbc23.Isyn = syn23.isynapse
syn24.Spre = pbc4.s + pbc7.s + pbc26.s + pbc31.s + pbc49.s + pbc50.s + pbc55.s + pbc62.s + pbc75.s + pbc94.s + pbc102.s 
pbc24.Isyn = syn24.isynapse
syn25.Spre = pbc4.s + pbc13.s + pbc18.s + pbc28.s + pbc41.s + pbc53.s + pbc76.s + pbc83.s + pbc92.s + pbc99.s + pbc102.s + pbc105.s 
pbc25.Isyn = syn25.isynapse
syn26.Spre = pbc4.s + pbc14.s + pbc20.s + pbc25.s + pbc35.s + pbc42.s + pbc67.s + pbc87.s + pbc104.s + pbc110.s + pbc121.s + pbc123.s 
pbc26.Isyn = syn26.isynapse
syn27.Spre = pbc4.s + pbc8.s + pbc15.s + pbc40.s + pbc55.s + pbc71.s + pbc102.s + pbc113.s 
pbc27.Isyn = syn27.isynapse
syn28.Spre = pbc12.s + pbc17.s + pbc20.s + pbc26.s + pbc47.s + pbc85.s + pbc97.s + pbc107.s 
pbc28.Isyn = syn28.isynapse
syn29.Spre = pbc4.s + pbc16.s + pbc32.s + pbc41.s + pbc50.s + pbc60.s + pbc61.s + pbc96.s + pbc110.s + pbc115.s + pbc117.s + pbc127.s + pbc128.s 
pbc29.Isyn = syn29.isynapse
syn30.Spre = pbc10.s + pbc12.s + pbc20.s + pbc22.s + pbc23.s + pbc24.s + pbc37.s + pbc45.s + pbc52.s + pbc66.s + pbc78.s + pbc81.s + pbc102.s + pbc106.s + pbc110.s + pbc111.s + pbc121.s + pbc122.s + pbc125.s 
pbc30.Isyn = syn30.isynapse
syn31.Spre = pbc10.s + pbc51.s + pbc58.s + pbc67.s + pbc68.s + pbc77.s + pbc90.s + pbc93.s 
pbc31.Isyn = syn31.isynapse
syn32.Spre = pbc1.s + pbc8.s + pbc14.s + pbc21.s + pbc45.s + pbc51.s + pbc58.s + pbc63.s + pbc94.s + pbc107.s + pbc108.s + pbc122.s + pbc126.s + pbc128.s 
pbc32.Isyn = syn32.isynapse
syn33.Spre = pbc46.s + pbc68.s + pbc70.s + pbc82.s + pbc85.s + pbc88.s + pbc107.s + pbc112.s 
pbc33.Isyn = syn33.isynapse
syn34.Spre = pbc2.s + pbc8.s + pbc9.s + pbc40.s + pbc46.s + pbc60.s + pbc61.s + pbc72.s + pbc77.s + pbc94.s + pbc126.s 
pbc34.Isyn = syn34.isynapse
syn35.Spre = pbc1.s + pbc29.s + pbc44.s + pbc47.s + pbc50.s + pbc52.s + pbc111.s + pbc123.s 
pbc35.Isyn = syn35.isynapse
syn36.Spre = pbc4.s + pbc6.s + pbc16.s + pbc45.s + pbc48.s + pbc55.s + pbc57.s + pbc84.s + pbc94.s + pbc114.s 
pbc36.Isyn = syn36.isynapse
syn37.Spre = pbc15.s + pbc17.s + pbc40.s + pbc45.s + pbc53.s + pbc58.s + pbc64.s + pbc70.s + pbc83.s + pbc84.s + pbc93.s + pbc116.s 
pbc37.Isyn = syn37.isynapse
syn38.Spre = pbc20.s + pbc43.s + pbc51.s + pbc52.s + pbc62.s + pbc71.s + pbc96.s + pbc103.s 
pbc38.Isyn = syn38.isynapse
syn39.Spre = pbc1.s + pbc29.s + pbc54.s + pbc59.s + pbc71.s + pbc77.s + pbc79.s + pbc84.s + pbc95.s 
pbc39.Isyn = syn39.isynapse
syn40.Spre = pbc4.s + pbc22.s + pbc49.s + pbc72.s + pbc75.s + pbc96.s + pbc100.s + pbc127.s 
pbc40.Isyn = syn40.isynapse
syn41.Spre = pbc2.s + pbc3.s + pbc11.s + pbc22.s + pbc24.s + pbc29.s + pbc31.s + pbc34.s + pbc38.s + pbc59.s + pbc60.s + pbc67.s + pbc75.s + pbc79.s + pbc82.s + pbc102.s + pbc127.s 
pbc41.Isyn = syn41.isynapse
syn42.Spre = pbc11.s + pbc17.s + pbc47.s + pbc67.s + pbc69.s + pbc82.s + pbc97.s + pbc98.s + pbc111.s + pbc116.s 
pbc42.Isyn = syn42.isynapse
syn43.Spre = pbc3.s + pbc6.s + pbc20.s + pbc34.s + pbc52.s + pbc58.s + pbc61.s + pbc69.s + pbc75.s + pbc85.s + pbc99.s + pbc118.s + pbc119.s 
pbc43.Isyn = syn43.isynapse
syn44.Spre = pbc14.s + pbc18.s + pbc24.s + pbc54.s + pbc57.s + pbc66.s + pbc69.s + pbc77.s + pbc114.s + pbc120.s 
pbc44.Isyn = syn44.isynapse
syn45.Spre = pbc6.s + pbc19.s + pbc38.s + pbc53.s + pbc55.s + pbc56.s + pbc81.s + pbc85.s + pbc116.s + pbc122.s + pbc123.s 
pbc45.Isyn = syn45.isynapse
syn46.Spre = pbc12.s + pbc16.s + pbc34.s + pbc37.s + pbc65.s + pbc73.s + pbc89.s + pbc97.s + pbc102.s + pbc104.s 
pbc46.Isyn = syn46.isynapse
syn47.Spre = pbc5.s + pbc32.s + pbc35.s + pbc64.s + pbc73.s + pbc94.s + pbc98.s + pbc108.s + pbc116.s + pbc125.s 
pbc47.Isyn = syn47.isynapse
syn48.Spre = pbc52.s + pbc62.s + pbc64.s + pbc76.s + pbc86.s + pbc93.s + pbc95.s + pbc105.s + pbc127.s 
pbc48.Isyn = syn48.isynapse
syn49.Spre = pbc10.s + pbc13.s + pbc16.s + pbc30.s + pbc46.s + pbc52.s + pbc65.s + pbc71.s + pbc87.s + pbc105.s + pbc121.s + pbc122.s + pbc124.s 
pbc49.Isyn = syn49.isynapse
syn50.Spre = pbc14.s + pbc15.s + pbc29.s + pbc31.s + pbc36.s + pbc57.s + pbc60.s + pbc63.s + pbc81.s + pbc91.s + pbc92.s + pbc102.s + pbc105.s + pbc125.s 
pbc50.Isyn = syn50.isynapse
syn51.Spre = pbc2.s + pbc27.s + pbc41.s + pbc95.s + pbc127.s 
pbc51.Isyn = syn51.isynapse
syn52.Spre = pbc5.s + pbc25.s + pbc27.s + pbc29.s + pbc30.s + pbc37.s + pbc38.s + pbc63.s + pbc80.s + pbc101.s + pbc107.s + pbc122.s 
pbc52.Isyn = syn52.isynapse
syn53.Spre = pbc4.s + pbc8.s + pbc14.s + pbc20.s + pbc34.s + pbc47.s + pbc60.s + pbc73.s + pbc84.s + pbc87.s + pbc107.s + pbc113.s 
pbc53.Isyn = syn53.isynapse
syn54.Spre = pbc19.s + pbc28.s + pbc33.s + pbc42.s + pbc63.s + pbc68.s + pbc99.s + pbc103.s + pbc104.s + pbc112.s + pbc113.s + pbc116.s + pbc119.s + pbc120.s 
pbc54.Isyn = syn54.isynapse
syn55.Spre = pbc2.s + pbc5.s + pbc13.s + pbc36.s + pbc39.s + pbc40.s + pbc83.s + pbc99.s + pbc104.s + pbc111.s + pbc124.s 
pbc55.Isyn = syn55.isynapse
syn56.Spre = pbc3.s + pbc23.s + pbc24.s + pbc48.s + pbc78.s + pbc89.s + pbc122.s 
pbc56.Isyn = syn56.isynapse
syn57.Spre = pbc5.s + pbc38.s + pbc43.s + pbc87.s + pbc118.s 
pbc57.Isyn = syn57.isynapse
syn58.Spre = pbc2.s + pbc16.s + pbc25.s + pbc26.s + pbc27.s + pbc84.s + pbc91.s + pbc102.s + pbc105.s + pbc120.s + pbc126.s + pbc128.s 
pbc58.Isyn = syn58.isynapse
syn59.Spre = pbc31.s + pbc38.s + pbc41.s + pbc76.s 
pbc59.Isyn = syn59.isynapse
syn60.Spre = pbc37.s + pbc44.s + pbc47.s + pbc71.s + pbc79.s + pbc82.s + pbc83.s + pbc103.s + pbc111.s + pbc115.s + pbc123.s 
pbc60.Isyn = syn60.isynapse
syn61.Spre = pbc1.s + pbc14.s + pbc52.s + pbc54.s + pbc77.s + pbc80.s + pbc83.s + pbc100.s + pbc110.s + pbc113.s + pbc128.s 
pbc61.Isyn = syn61.isynapse
syn62.Spre = pbc9.s + pbc13.s + pbc35.s + pbc72.s + pbc86.s + pbc111.s + pbc115.s + pbc123.s 
pbc62.Isyn = syn62.isynapse
syn63.Spre = pbc1.s + pbc8.s + pbc21.s + pbc28.s + pbc64.s + pbc89.s + pbc92.s + pbc104.s + pbc105.s + pbc106.s + pbc108.s + pbc116.s + pbc123.s 
pbc63.Isyn = syn63.isynapse
syn64.Spre = pbc21.s + pbc34.s + pbc43.s + pbc53.s + pbc76.s + pbc91.s + pbc103.s + pbc115.s + pbc117.s 
pbc64.Isyn = syn64.isynapse
syn65.Spre = pbc13.s + pbc25.s + pbc63.s + pbc68.s + pbc90.s + pbc118.s + pbc119.s + pbc122.s + pbc124.s 
pbc65.Isyn = syn65.isynapse
syn66.Spre = pbc1.s + pbc10.s + pbc48.s + pbc57.s + pbc64.s + pbc68.s + pbc73.s + pbc93.s + pbc105.s + pbc118.s 
pbc66.Isyn = syn66.isynapse
syn67.Spre = pbc3.s + pbc8.s + pbc12.s + pbc13.s + pbc21.s + pbc36.s + pbc38.s + pbc53.s + pbc58.s + pbc106.s + pbc123.s 
pbc67.Isyn = syn67.isynapse
syn68.Spre = pbc10.s + pbc16.s + pbc33.s + pbc41.s + pbc43.s + pbc60.s + pbc77.s + pbc109.s + pbc110.s 
pbc68.Isyn = syn68.isynapse
syn69.Spre = pbc14.s + pbc56.s + pbc74.s + pbc90.s + pbc91.s + pbc116.s 
pbc69.Isyn = syn69.isynapse
syn70.Spre = pbc21.s + pbc29.s + pbc32.s + pbc46.s + pbc47.s + pbc57.s + pbc62.s + pbc109.s + pbc116.s + pbc125.s + pbc128.s 
pbc70.Isyn = syn70.isynapse
syn71.Spre = pbc11.s + pbc44.s + pbc52.s + pbc101.s + pbc110.s + pbc112.s + pbc128.s 
pbc71.Isyn = syn71.isynapse
syn72.Spre = pbc9.s + pbc14.s + pbc45.s + pbc106.s + pbc118.s 
pbc72.Isyn = syn72.isynapse
syn73.Spre = pbc23.s + pbc25.s + pbc39.s + pbc85.s + pbc91.s + pbc96.s + pbc97.s + pbc108.s + pbc109.s + pbc121.s 
pbc73.Isyn = syn73.isynapse
syn74.Spre = pbc2.s + pbc3.s + pbc7.s + pbc13.s + pbc28.s + pbc29.s + pbc37.s + pbc59.s + pbc62.s + pbc63.s + pbc83.s + pbc87.s + pbc109.s + pbc126.s 
pbc74.Isyn = syn74.isynapse
syn75.Spre = pbc1.s + pbc8.s + pbc42.s + pbc43.s + pbc52.s + pbc82.s + pbc97.s + pbc123.s + pbc125.s 
pbc75.Isyn = syn75.isynapse
syn76.Spre = pbc7.s + pbc18.s + pbc27.s + pbc30.s + pbc33.s + pbc38.s + pbc50.s + pbc51.s + pbc54.s + pbc80.s + pbc98.s + pbc121.s 
pbc76.Isyn = syn76.isynapse
syn77.Spre = pbc6.s + pbc32.s + pbc38.s + pbc68.s + pbc80.s + pbc96.s + pbc97.s + pbc114.s 
pbc77.Isyn = syn77.isynapse
syn78.Spre = pbc17.s + pbc18.s + pbc47.s + pbc54.s + pbc82.s + pbc96.s 
pbc78.Isyn = syn78.isynapse
syn79.Spre = pbc11.s + pbc18.s + pbc21.s + pbc24.s + pbc50.s + pbc57.s + pbc70.s + pbc89.s + pbc101.s + pbc122.s 
pbc79.Isyn = syn79.isynapse
syn80.Spre = pbc5.s + pbc28.s + pbc35.s + pbc41.s + pbc47.s + pbc65.s + pbc97.s + pbc114.s + pbc119.s 
pbc80.Isyn = syn80.isynapse
syn81.Spre = pbc6.s + pbc11.s + pbc20.s + pbc26.s + pbc29.s + pbc31.s + pbc35.s + pbc37.s + pbc42.s + pbc64.s + pbc82.s + pbc86.s + pbc87.s + pbc92.s + pbc112.s 
pbc81.Isyn = syn81.isynapse
syn82.Spre = pbc22.s + pbc39.s + pbc53.s + pbc67.s + pbc88.s + pbc97.s + pbc117.s 
pbc82.Isyn = syn82.isynapse
syn83.Spre = pbc3.s + pbc19.s + pbc46.s + pbc92.s + pbc94.s + pbc95.s + pbc101.s + pbc109.s + pbc125.s 
pbc83.Isyn = syn83.isynapse
syn84.Spre = pbc15.s + pbc45.s + pbc49.s + pbc53.s + pbc66.s + pbc69.s + pbc74.s + pbc98.s + pbc111.s + pbc118.s 
pbc84.Isyn = syn84.isynapse
syn85.Spre = pbc19.s + pbc35.s + pbc50.s + pbc51.s + pbc54.s + pbc66.s + pbc101.s + pbc105.s 
pbc85.Isyn = syn85.isynapse
syn86.Spre = pbc12.s + pbc17.s + pbc21.s + pbc26.s + pbc34.s + pbc37.s + pbc90.s + pbc96.s + pbc99.s + pbc111.s + pbc122.s + pbc124.s 
pbc86.Isyn = syn86.isynapse
syn87.Spre = pbc15.s + pbc23.s + pbc24.s + pbc26.s + pbc60.s + pbc64.s + pbc69.s + pbc72.s + pbc98.s 
pbc87.Isyn = syn87.isynapse
syn88.Spre = pbc2.s + pbc9.s + pbc19.s + pbc22.s + pbc39.s + pbc55.s + pbc72.s + pbc73.s + pbc94.s + pbc100.s 
pbc88.Isyn = syn88.isynapse
syn89.Spre = pbc6.s + pbc55.s + pbc63.s + pbc67.s + pbc77.s + pbc81.s + pbc100.s + pbc102.s 
pbc89.Isyn = syn89.isynapse
syn90.Spre = pbc2.s + pbc30.s + pbc47.s + pbc51.s 
pbc90.Isyn = syn90.isynapse
syn91.Spre = pbc1.s + pbc7.s + pbc16.s + pbc30.s + pbc33.s + pbc49.s + pbc51.s + pbc73.s + pbc110.s + pbc111.s 
pbc91.Isyn = syn91.isynapse
syn92.Spre = pbc29.s + pbc30.s + pbc39.s + pbc64.s + pbc65.s 
pbc92.Isyn = syn92.isynapse
syn93.Spre = pbc4.s + pbc5.s + pbc20.s + pbc49.s + pbc56.s + pbc71.s + pbc100.s + pbc121.s 
pbc93.Isyn = syn93.isynapse
syn94.Spre = pbc13.s + pbc14.s + pbc16.s + pbc66.s + pbc80.s 
pbc94.Isyn = syn94.isynapse
syn95.Spre = pbc5.s + pbc10.s + pbc17.s + pbc33.s + pbc69.s + pbc70.s + pbc97.s + pbc106.s + pbc109.s + pbc121.s + pbc123.s + pbc126.s 
pbc95.Isyn = syn95.isynapse
syn96.Spre = pbc3.s + pbc8.s + pbc31.s + pbc86.s + pbc115.s + pbc120.s + pbc126.s 
pbc96.Isyn = syn96.isynapse
syn97.Spre = pbc17.s + pbc28.s + pbc44.s + pbc50.s + pbc56.s + pbc59.s + pbc89.s + pbc128.s 
pbc97.Isyn = syn97.isynapse
syn98.Spre = pbc6.s + pbc14.s + pbc27.s + pbc40.s + pbc43.s + pbc55.s + pbc81.s + pbc104.s + pbc112.s + pbc120.s + pbc128.s 
pbc98.Isyn = syn98.isynapse
syn99.Spre = pbc26.s + pbc46.s + pbc47.s + pbc48.s + pbc56.s + pbc64.s + pbc75.s + pbc77.s + pbc78.s + pbc93.s + pbc106.s + pbc122.s 
pbc99.Isyn = syn99.isynapse
syn100.Spre = pbc24.s + pbc27.s + pbc33.s + pbc34.s + pbc37.s + pbc49.s + pbc56.s + pbc80.s + pbc107.s + pbc112.s 
pbc100.Isyn = syn100.isynapse
syn101.Spre = pbc9.s + pbc15.s + pbc36.s + pbc45.s + pbc61.s + pbc66.s + pbc81.s + pbc124.s + pbc127.s 
pbc101.Isyn = syn101.isynapse
syn102.Spre = pbc9.s + pbc19.s + pbc32.s + pbc58.s + pbc65.s + pbc76.s + pbc81.s + pbc96.s + pbc116.s + pbc118.s + pbc124.s + pbc127.s 
pbc102.Isyn = syn102.isynapse
syn103.Spre = pbc18.s + pbc32.s + pbc38.s + pbc49.s + pbc68.s + pbc88.s + pbc90.s 
pbc103.Isyn = syn103.isynapse
syn104.Spre = pbc9.s + pbc18.s + pbc28.s + pbc39.s + pbc50.s + pbc66.s + pbc84.s + pbc125.s 
pbc104.Isyn = syn104.isynapse
syn105.Spre = pbc23.s + pbc36.s + pbc39.s + pbc78.s + pbc84.s 
pbc105.Isyn = syn105.isynapse
syn106.Spre = pbc6.s + pbc24.s + pbc25.s + pbc48.s + pbc51.s + pbc53.s + pbc68.s + pbc79.s + pbc86.s 
pbc106.Isyn = syn106.isynapse
syn107.Spre = pbc22.s + pbc24.s + pbc36.s + pbc57.s + pbc63.s + pbc69.s + pbc76.s + pbc83.s + pbc115.s + pbc117.s + pbc121.s + pbc123.s 
pbc107.Isyn = syn107.isynapse
syn108.Spre = pbc17.s + pbc27.s + pbc46.s + pbc54.s + pbc56.s + pbc71.s + pbc83.s + pbc85.s + pbc88.s + pbc115.s + pbc119.s + pbc126.s 
pbc108.Isyn = syn108.isynapse
syn109.Spre = pbc11.s + pbc35.s + pbc59.s + pbc63.s + pbc89.s + pbc92.s + pbc99.s + pbc112.s + pbc114.s + pbc118.s 
pbc109.Isyn = syn109.isynapse
syn110.Spre = pbc6.s + pbc10.s + pbc15.s + pbc19.s + pbc36.s + pbc37.s + pbc48.s + pbc79.s + pbc81.s + pbc91.s + pbc95.s + pbc103.s + pbc114.s + pbc120.s 
pbc110.Isyn = syn110.isynapse
syn111.Spre = pbc8.s + pbc49.s + pbc61.s + pbc70.s + pbc73.s + pbc76.s + pbc81.s + pbc86.s + pbc105.s 
pbc111.Isyn = syn111.isynapse
syn112.Spre = pbc22.s + pbc35.s + pbc41.s + pbc45.s + pbc53.s + pbc54.s + pbc78.s + pbc91.s + pbc98.s + pbc114.s 
pbc112.Isyn = syn112.isynapse
syn113.Spre = pbc9.s + pbc12.s + pbc32.s + pbc42.s + pbc72.s + pbc74.s + pbc104.s + pbc126.s 
pbc113.Isyn = syn113.isynapse
syn114.Spre = pbc17.s + pbc19.s + pbc36.s + pbc44.s + pbc59.s + pbc60.s + pbc79.s + pbc82.s + pbc90.s + pbc100.s + pbc104.s + pbc117.s + pbc119.s 
pbc114.Isyn = syn114.isynapse
syn115.Spre = pbc3.s + pbc18.s + pbc52.s + pbc56.s + pbc58.s + pbc65.s + pbc78.s + pbc81.s + pbc85.s + pbc89.s + pbc91.s + pbc119.s 
pbc115.Isyn = syn115.isynapse
syn116.Spre = pbc19.s + pbc23.s + pbc27.s + pbc40.s + pbc53.s + pbc57.s + pbc62.s + pbc65.s + pbc72.s + pbc78.s + pbc79.s + pbc86.s + pbc101.s + pbc109.s + pbc125.s 
pbc116.Isyn = syn116.isynapse
syn117.Spre = pbc27.s + pbc43.s + pbc59.s + pbc62.s + pbc86.s + pbc87.s + pbc104.s + pbc124.s + pbc127.s 
pbc117.Isyn = syn117.isynapse
syn118.Spre = pbc3.s + pbc13.s + pbc44.s + pbc48.s + pbc49.s + pbc64.s + pbc74.s + pbc79.s + pbc85.s + pbc87.s + pbc92.s + pbc94.s + pbc101.s + pbc104.s + pbc112.s + pbc117.s + pbc121.s 
pbc118.Isyn = syn118.isynapse
syn119.Spre = pbc11.s + pbc12.s + pbc18.s + pbc31.s + pbc39.s + pbc56.s + pbc65.s + pbc75.s + pbc76.s + pbc77.s + pbc82.s + pbc89.s + pbc109.s + pbc110.s + pbc113.s 
pbc119.Isyn = syn119.isynapse
syn120.Spre = pbc5.s + pbc15.s + pbc27.s + pbc31.s + pbc33.s + pbc73.s + pbc93.s + pbc103.s + pbc121.s 
pbc120.Isyn = syn120.isynapse
syn121.Spre = pbc1.s + pbc22.s + pbc34.s + pbc40.s + pbc42.s + pbc61.s + pbc70.s + pbc72.s + pbc88.s + pbc93.s + pbc101.s + pbc106.s + pbc108.s + pbc127.s 
pbc121.Isyn = syn121.isynapse
syn122.Spre = pbc1.s + pbc10.s + pbc16.s + pbc19.s + pbc72.s + pbc75.s + pbc76.s + pbc98.s + pbc100.s 
pbc122.Isyn = syn122.isynapse
syn123.Spre = pbc7.s + pbc18.s + pbc23.s + pbc32.s + pbc36.s + pbc40.s + pbc42.s + pbc48.s + pbc58.s + pbc91.s + pbc102.s + pbc111.s + pbc113.s + pbc124.s 
pbc123.Isyn = syn123.isynapse
syn124.Spre = pbc3.s + pbc13.s + pbc15.s + pbc40.s + pbc58.s + pbc80.s 
pbc124.Isyn = syn124.isynapse
syn125.Spre = pbc34.s + pbc50.s + pbc57.s + pbc65.s + pbc66.s + pbc71.s + pbc74.s + pbc78.s + pbc91.s + pbc103.s 
pbc125.Isyn = syn125.isynapse
syn126.Spre = pbc5.s + pbc37.s + pbc43.s + pbc44.s + pbc59.s + pbc67.s + pbc69.s + pbc84.s + pbc86.s + pbc93.s + pbc99.s + pbc108.s + pbc114.s + pbc120.s 
pbc126.Isyn = syn126.isynapse
syn127.Spre = pbc26.s + pbc39.s + pbc42.s + pbc56.s + pbc74.s + pbc92.s + pbc95.s + pbc108.s + pbc110.s 
pbc127.Isyn = syn127.isynapse
syn128.Spre = pbc7.s + pbc11.s + pbc28.s + pbc45.s + pbc94.s + pbc97.s + pbc99.s + pbc112.s + pbc117.s + pbc120.s 
pbc128.Isyn = syn128.isynapse

println "Generating voltage outputs"

output Vm1 = pbc1.Vm
//output Vm1 = pbc1.spiketime
output Vm2 = pbc2.Vm
output Vm3 = pbc3.Vm
output Vm4 = pbc4.Vm
output Vm5 = pbc5.Vm
output Vm6 = pbc6.Vm
output Vm7 = pbc7.Vm
output Vm8 = pbc8.Vm
output Vm9 = pbc9.Vm
output Vm10 = pbc10.Vm
output Vm11 = pbc11.Vm
output Vm12 = pbc12.Vm
output Vm13 = pbc13.Vm
output Vm14 = pbc14.Vm
output Vm15 = pbc15.Vm
output Vm16 = pbc16.Vm
output Vm17 = pbc17.Vm
output Vm18 = pbc18.Vm
output Vm19 = pbc19.Vm
output Vm20 = pbc20.Vm
output Vm21 = pbc21.Vm
output Vm22 = pbc22.Vm
output Vm23 = pbc23.Vm
output Vm24 = pbc24.Vm
output Vm25 = pbc25.Vm
output Vm26 = pbc26.Vm
output Vm27 = pbc27.Vm
output Vm28 = pbc28.Vm
output Vm29 = pbc29.Vm
output Vm30 = pbc30.Vm
output Vm31 = pbc31.Vm
output Vm32 = pbc32.Vm
output Vm33 = pbc33.Vm
output Vm34 = pbc34.Vm
output Vm35 = pbc35.Vm
output Vm36 = pbc36.Vm
output Vm37 = pbc37.Vm
output Vm38 = pbc38.Vm
output Vm39 = pbc39.Vm
output Vm40 = pbc40.Vm
output Vm41 = pbc41.Vm
output Vm42 = pbc42.Vm
output Vm43 = pbc43.Vm
output Vm44 = pbc44.Vm
output Vm45 = pbc45.Vm
output Vm46 = pbc46.Vm
output Vm47 = pbc47.Vm
output Vm48 = pbc48.Vm
output Vm49 = pbc49.Vm
output Vm50 = pbc50.Vm
output Vm51 = pbc51.Vm
output Vm52 = pbc52.Vm
output Vm53 = pbc53.Vm
output Vm54 = pbc54.Vm
output Vm55 = pbc55.Vm
output Vm56 = pbc56.Vm
output Vm57 = pbc57.Vm
output Vm58 = pbc58.Vm
output Vm59 = pbc59.Vm
output Vm60 = pbc60.Vm
output Vm61 = pbc61.Vm
output Vm62 = pbc62.Vm
output Vm63 = pbc63.Vm
output Vm64 = pbc64.Vm
output Vm65 = pbc65.Vm
output Vm66 = pbc66.Vm
output Vm67 = pbc67.Vm
output Vm68 = pbc68.Vm
output Vm69 = pbc69.Vm
output Vm70 = pbc70.Vm
output Vm71 = pbc71.Vm
output Vm72 = pbc72.Vm
output Vm73 = pbc73.Vm
output Vm74 = pbc74.Vm
output Vm75 = pbc75.Vm
output Vm76 = pbc76.Vm
output Vm77 = pbc77.Vm
output Vm78 = pbc78.Vm
output Vm79 = pbc79.Vm
output Vm80 = pbc80.Vm
output Vm81 = pbc81.Vm
output Vm82 = pbc82.Vm
output Vm83 = pbc83.Vm
output Vm84 = pbc84.Vm
output Vm85 = pbc85.Vm
output Vm86 = pbc86.Vm
output Vm87 = pbc87.Vm
output Vm88 = pbc88.Vm
output Vm89 = pbc89.Vm
output Vm90 = pbc90.Vm
output Vm91 = pbc91.Vm
output Vm92 = pbc92.Vm
output Vm93 = pbc93.Vm
output Vm94 = pbc94.Vm
output Vm95 = pbc95.Vm
output Vm96 = pbc96.Vm
output Vm97 = pbc97.Vm
output Vm98 = pbc98.Vm
output Vm99 = pbc99.Vm
output Vm100 = pbc100.Vm
output Vm101 = pbc101.Vm
output Vm102 = pbc102.Vm
output Vm103 = pbc103.Vm
output Vm104 = pbc104.Vm
output Vm105 = pbc105.Vm
output Vm106 = pbc106.Vm
output Vm107 = pbc107.Vm
output Vm108 = pbc108.Vm
output Vm109 = pbc109.Vm
output Vm110 = pbc110.Vm
output Vm111 = pbc111.Vm
output Vm112 = pbc112.Vm
output Vm113 = pbc113.Vm
output Vm114 = pbc114.Vm
output Vm115 = pbc115.Vm
output Vm116 = pbc116.Vm
output Vm117 = pbc117.Vm
output Vm118 = pbc118.Vm
output Vm119 = pbc119.Vm
output Vm120 = pbc120.Vm
output Vm121 = pbc121.Vm
output Vm122 = pbc122.Vm
output Vm123 = pbc123.Vm
output Vm124 = pbc124.Vm
output Vm125 = pbc125.Vm
output Vm126 = pbc126.Vm
output Vm127 = pbc127.Vm
output Vm128 = pbc128.Vm

println "Assigning solver"

solver = ode23
solver.dt = 0.001

println "Done with pbcNucleus"

end
println "out of file"