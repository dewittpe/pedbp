// Generated by data-raw/sysdata.R
// Do not edit by hand
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "lms.h"
arma::mat stature_for_age_cdc_female() {
	arma::mat LMS = {
{ 0, -1.295960857, 49.28639612, 0.05008556 }, 
{ 0.5, -0.809249882, 51.68358057, 0.046818545 }, 
{ 1.5, -0.050782985, 55.28612813, 0.0434439 }, 
{ 2.5, 0.476851407, 58.09381906, 0.041716103 }, 
{ 3.5, 0.843299612, 60.45980763, 0.040705173 }, 
{ 4.5, 1.097562257, 62.53669656, 0.040079765 }, 
{ 5.5, 1.272509641, 64.40632762, 0.039686845 }, 
{ 6.5, 1.390428859, 66.11841553, 0.039444555 }, 
{ 7.5, 1.466733925, 67.70574419, 0.039304738 }, 
{ 8.5, 1.512301976, 69.19123614, 0.03923711 }, 
{ 9.5, 1.534950767, 70.59163924, 0.039221665 }, 
{ 10.5, 1.540390875, 71.91961673, 0.039244672 }, 
{ 11.5, 1.532852892, 73.1850104, 0.03929642 }, 
{ 12.5, 1.51550947, 74.39564379, 0.039369875 }, 
{ 13.5, 1.490765028, 75.5578544, 0.039459832 }, 
{ 14.5, 1.460458255, 76.67685871, 0.039562382 }, 
{ 15.5, 1.426006009, 77.75700986, 0.039674542 }, 
{ 16.5, 1.388507095, 78.80198406, 0.03979401 }, 
{ 17.5, 1.348818127, 79.81491852, 0.039918994 }, 
{ 18.5, 1.307609654, 80.79851532, 0.040048084 }, 
{ 19.5, 1.265408149, 81.75512092, 0.040180162 }, 
{ 20.5, 1.222627732, 82.6867881, 0.04031434 }, 
{ 21.5, 1.179594365, 83.59532461, 0.040449904 }, 
{ 22.5, 1.136564448, 84.48233206, 0.040586283 }, 
{ 23.5, 1.093731947, 85.34923624, 0.040723015 }, 
{ 24, 1.07244896, 84.97555512, 0.040791394 }, 
{ 24.5, 1.051272912, 86.1973169, 0.040859727 }, 
{ 24.5, 1.051272912, 85.3973169, 0.040859727 }, 
{ 25.5, 1.041951175, 87.09026318, 0.041142161 }, 
{ 25.5, 1.041951175, 86.29026318, 0.041142161 }, 
{ 26.5, 1.012592236, 87.95714182, 0.041349399 }, 
{ 26.5, 1.012592236, 87.15714182, 0.041349399 }, 
{ 27.5, 0.970541909, 88.7960184, 0.041500428 }, 
{ 27.5, 0.970541909, 87.9960184, 0.041500428 }, 
{ 28.5, 0.921129988, 89.6055115, 0.041610508 }, 
{ 28.5, 0.921129988, 88.8055115, 0.041610508 }, 
{ 29.5, 0.868221392, 90.38476689, 0.041691761 }, 
{ 29.5, 0.868221392, 89.58476689, 0.041691761 }, 
{ 30.5, 0.81454413, 91.13341722, 0.04175368 }, 
{ 30.5, 0.81454413, 90.33341722, 0.04175368 }, 
{ 31.5, 0.761957977, 91.8515436, 0.041803562 }, 
{ 31.5, 0.761957977, 91.0515436, 0.041803562 }, 
{ 32.5, 0.711660228, 92.5396352, 0.041846882 }, 
{ 32.5, 0.711660228, 91.7396352, 0.041846882 }, 
{ 33.5, 0.664323379, 93.19854429, 0.041887626 }, 
{ 33.5, 0.664323379, 92.39854429, 0.041887626 }, 
{ 34.5, 0.620285102, 93.82945392, 0.041928568 }, 
{ 34.5, 0.620285102, 93.02945392, 0.041928568 }, 
{ 35.5, 0.57955631, 94.43382278, 0.041971514 }, 
{ 35.5, 0.57955631, 93.63382278, 0.041971514 }, 
{ 36.5, 0.54198094, 94.21335709, 0.042017509 }, 
{ 37.5, 0.511429832, 94.79643239, 0.042104522 }, 
{ 38.5, 0.482799937, 95.37391918, 0.042199507 }, 
{ 39.5, 0.455521041, 95.94692677, 0.042300333 }, 
{ 40.5, 0.429150288, 96.51644912, 0.042405225 }, 
{ 41.5, 0.403351725, 97.08337211, 0.042512706 }, 
{ 42.5, 0.377878239, 97.6484807, 0.042621565 }, 
{ 43.5, 0.352555862, 98.21246579, 0.042730809 }, 
{ 44.5, 0.327270297, 98.77593069, 0.042839638 }, 
{ 45.5, 0.301955463, 99.33939735, 0.042947412 }, 
{ 46.5, 0.276583851, 99.9033122, 0.043053626 }, 
{ 47.5, 0.251158446, 100.4680516, 0.043157889 }, 
{ 48.5, 0.225705996, 101.033927, 0.043259907 }, 
{ 49.5, 0.20027145, 101.6011898, 0.043359463 }, 
{ 50.5, 0.174913356, 102.1700358, 0.043456406 }, 
{ 51.5, 0.149700081, 102.7406094, 0.043550638 }, 
{ 52.5, 0.12470671, 103.3130077, 0.043642107 }, 
{ 53.5, 0.100012514, 103.8872839, 0.043730791 }, 
{ 54.5, 0.075698881, 104.4634511, 0.043816701 }, 
{ 55.5, 0.051847635, 105.0414853, 0.043899867 }, 
{ 56.5, 0.02853967, 105.6213287, 0.043980337 }, 
{ 57.5, 0.005853853, 106.2028921, 0.044058171 }, 
{ 58.5, -0.016133871, 106.7860583, 0.04413344 }, 
{ 59.5, -0.037351181, 107.3706841, 0.044206218 }, 
{ 60.5, -0.057729947, 107.9566031, 0.044276588 }, 
{ 61.5, -0.077206672, 108.5436278, 0.044344632 }, 
{ 62.5, -0.09572283, 109.1315521, 0.044410436 }, 
{ 63.5, -0.113225128, 109.7201531, 0.044474084 }, 
{ 64.5, -0.129665689, 110.3091934, 0.044535662 }, 
{ 65.5, -0.145002179, 110.8984228, 0.044595254 }, 
{ 66.5, -0.159197885, 111.4875806, 0.044652942 }, 
{ 67.5, -0.172221748, 112.0763967, 0.044708809 }, 
{ 68.5, -0.184048358, 112.6645943, 0.044762936 }, 
{ 69.5, -0.194660215, 113.2518902, 0.044815402 }, 
{ 70.5, -0.204030559, 113.8380006, 0.044866288 }, 
{ 71.5, -0.212174408, 114.4226317, 0.044915672 }, 
{ 72.5, -0.219069129, 115.0054978, 0.044963636 }, 
{ 73.5, -0.224722166, 115.5863089, 0.045010259 }, 
{ 74.5, -0.229140412, 116.1647782, 0.045055624 }, 
{ 75.5, -0.232335686, 116.7406221, 0.045099817 }, 
{ 76.5, -0.234324563, 117.3135622, 0.045142924 }, 
{ 77.5, -0.235128195, 117.8833259, 0.045185036 }, 
{ 78.5, -0.234772114, 118.4496481, 0.045226249 }, 
{ 79.5, -0.233286033, 119.0122722, 0.045266662 }, 
{ 80.5, -0.230703633, 119.5709513, 0.045306383 }, 
{ 81.5, -0.227062344, 120.1254495, 0.045345524 }, 
{ 82.5, -0.222403111, 120.6755427, 0.045384203 }, 
{ 83.5, -0.216770161, 121.22102, 0.045422551 }, 
{ 84.5, -0.210210748, 121.7616844, 0.045460702 }, 
{ 85.5, -0.202774891, 122.2973542, 0.045498803 }, 
{ 86.5, -0.194515104, 122.827864, 0.045537012 }, 
{ 87.5, -0.185486099, 123.3530652, 0.045575495 }, 
{ 88.5, -0.175744476, 123.8728276, 0.045614432 }, 
{ 89.5, -0.165348396, 124.38704, 0.045654016 }, 
{ 90.5, -0.15435722, 124.8956114, 0.04569445 }, 
{ 91.5, -0.142831123, 125.398472, 0.045735953 }, 
{ 92.5, -0.130830669, 125.895574, 0.045778759 }, 
{ 93.5, -0.118416354, 126.3868929, 0.045823114 }, 
{ 94.5, -0.105648092, 126.8724284, 0.04586928 }, 
{ 95.5, -0.092584657, 127.3522056, 0.045917535 }, 
{ 96.5, -0.079283065, 127.8262759, 0.045968169 }, 
{ 97.5, -0.065797888, 128.2947187, 0.04602149 }, 
{ 98.5, -0.0521805, 128.757642, 0.046077818 }, 
{ 99.5, -0.03847825, 129.2151839, 0.046137487 }, 
{ 100.5, -0.024733545, 129.6675143, 0.046200842 }, 
{ 101.5, -0.010982868, 130.1148354, 0.04626824 }, 
{ 102.5, 0.002744306, 130.5573839, 0.046340046 }, 
{ 103.5, 0.016426655, 130.995432, 0.046416629 }, 
{ 104.5, 0.030052231, 131.4292887, 0.046498361 }, 
{ 105.5, 0.043619747, 131.8593015, 0.046585611 }, 
{ 106.5, 0.05713988, 132.2858574, 0.046678741 }, 
{ 107.5, 0.070636605, 132.7093845, 0.046778099 }, 
{ 108.5, 0.08414848, 133.1303527, 0.04688401 }, 
{ 109.5, 0.097729873, 133.5492749, 0.046996769 }, 
{ 110.5, 0.111452039, 133.9667073, 0.047116633 }, 
{ 111.5, 0.125404005, 134.3832499, 0.047243801 }, 
{ 112.5, 0.13969316, 134.7995463, 0.047378413 }, 
{ 113.5, 0.154445482, 135.2162826, 0.047520521 }, 
{ 114.5, 0.169805275, 135.634186, 0.047670085 }, 
{ 115.5, 0.185934346, 136.0540223, 0.047826946 }, 
{ 116.5, 0.203010488, 136.4765925, 0.04799081 }, 
{ 117.5, 0.2212252, 136.9027281, 0.048161228 }, 
{ 118.5, 0.240780542, 137.3332846, 0.04833757 }, 
{ 119.5, 0.261885086, 137.7691339, 0.048519011 }, 
{ 120.5, 0.284748919, 138.2111552, 0.048704503 }, 
{ 121.5, 0.309577733, 138.6602228, 0.048892759 }, 
{ 122.5, 0.336566048, 139.1171933, 0.049082239 }, 
{ 123.5, 0.365889711, 139.5828898, 0.049271137 }, 
{ 124.5, 0.397699038, 140.0580848, 0.049457371 }, 
{ 125.5, 0.432104409, 140.5434787, 0.049638596 }, 
{ 126.5, 0.46917993, 141.0396832, 0.049812203 }, 
{ 127.5, 0.508943272, 141.5471945, 0.049975355 }, 
{ 128.5, 0.551354277, 142.0663731, 0.050125012 }, 
{ 129.5, 0.596307363, 142.59742, 0.050257992 }, 
{ 130.5, 0.643626542, 143.1403553, 0.050371024 }, 
{ 131.5, 0.693062173, 143.6949981, 0.050460835 }, 
{ 132.5, 0.744289752, 144.2609497, 0.050524236 }, 
{ 133.5, 0.79691098, 144.8375809, 0.050558224 }, 
{ 134.5, 0.85045728, 145.4240246, 0.050560083 }, 
{ 135.5, 0.904395871, 146.0191748, 0.050527494 }, 
{ 136.5, 0.958138449, 146.621692, 0.050458634 }, 
{ 137.5, 1.011054559, 147.2300177, 0.050352269 }, 
{ 138.5, 1.062474568, 147.8423918, 0.050207825 }, 
{ 139.5, 1.111727029, 148.4568879, 0.050025434 }, 
{ 140.5, 1.158135105, 149.0714413, 0.049805967 }, 
{ 141.5, 1.201050821, 149.6838943, 0.049551023 }, 
{ 142.5, 1.239852328, 150.2920328, 0.049262895 }, 
{ 143.5, 1.274006058, 150.8936469, 0.048944504 }, 
{ 144.5, 1.303044695, 151.4865636, 0.048599314 }, 
{ 145.5, 1.326605954, 152.0686985, 0.048231224 }, 
{ 146.5, 1.344443447, 152.6380955, 0.047844442 }, 
{ 147.5, 1.356437773, 153.1929631, 0.047443362 }, 
{ 148.5, 1.362602695, 153.7317031, 0.04703243 }, 
{ 149.5, 1.363085725, 154.2529332, 0.046616026 }, 
{ 150.5, 1.358162799, 154.755501, 0.046198356 }, 
{ 151.5, 1.348227142, 155.2384904, 0.04578335 }, 
{ 152.5, 1.333772923, 155.7012216, 0.045374597 }, 
{ 153.5, 1.315374704, 156.1432438, 0.044975281 }, 
{ 154.5, 1.293664024, 156.564323, 0.044588148 }, 
{ 155.5, 1.269304678, 156.9644258, 0.044215488 }, 
{ 156.5, 1.242968236, 157.3436995, 0.043859135 }, 
{ 157.5, 1.21531127, 157.7024507, 0.04352048 }, 
{ 158.5, 1.186955477, 158.0411233, 0.043200497 }, 
{ 159.5, 1.158471522, 158.3602756, 0.042899776 }, 
{ 160.5, 1.130367088, 158.6605588, 0.042618565 }, 
{ 161.5, 1.103079209, 158.9426964, 0.042356812 }, 
{ 162.5, 1.076970655, 159.2074654, 0.042114211 }, 
{ 163.5, 1.052329922, 159.455679, 0.041890247 }, 
{ 164.5, 1.029374161, 159.688172, 0.04168424 }, 
{ 165.5, 1.008254396, 159.9057871, 0.041495379 }, 
{ 166.5, 0.989062282, 160.1093647, 0.041322765 }, 
{ 167.5, 0.971837799, 160.299733, 0.041165437 }, 
{ 168.5, 0.95657215, 160.4776996, 0.041022401 }, 
{ 169.5, 0.94324228, 160.6440526, 0.040892651 }, 
{ 170.5, 0.931767062, 160.7995428, 0.040775193 }, 
{ 171.5, 0.922058291, 160.9448916, 0.040669052 }, 
{ 172.5, 0.914012643, 161.0807857, 0.040573288 }, 
{ 173.5, 0.907516917, 161.2078755, 0.040487005 }, 
{ 174.5, 0.902452436, 161.3267744, 0.040409354 }, 
{ 175.5, 0.898698641, 161.4380593, 0.040339537 }, 
{ 176.5, 0.896143482, 161.5422726, 0.040276811 }, 
{ 177.5, 0.894659668, 161.639917, 0.040220488 }, 
{ 178.5, 0.89413892, 161.7314645, 0.040169932 }, 
{ 179.5, 0.894475371, 161.8173534, 0.040124562 }, 
{ 180.5, 0.895569834, 161.8979913, 0.040083845 }, 
{ 181.5, 0.897330209, 161.9737558, 0.040047295 }, 
{ 182.5, 0.899671635, 162.0449969, 0.040014473 }, 
{ 183.5, 0.902516442, 162.1120386, 0.03998498 }, 
{ 184.5, 0.905793969, 162.17518, 0.039958458 }, 
{ 185.5, 0.909440266, 162.2346979, 0.039934584 }, 
{ 186.5, 0.913397733, 162.2908474, 0.039913066 }, 
{ 187.5, 0.91761471, 162.343864, 0.039893644 }, 
{ 188.5, 0.922045055, 162.3939652, 0.039876087 }, 
{ 189.5, 0.926647697, 162.4413513, 0.039860185 }, 
{ 190.5, 0.931386217, 162.4862071, 0.039845754 }, 
{ 191.5, 0.93622842, 162.5287029, 0.039832629 }, 
{ 192.5, 0.941145943, 162.5689958, 0.039820663 }, 
{ 193.5, 0.94611388, 162.6072309, 0.039809725 }, 
{ 194.5, 0.95111043, 162.6435418, 0.0397997 }, 
{ 195.5, 0.956116576, 162.6780519, 0.039790485 }, 
{ 196.5, 0.961115792, 162.7108751, 0.039781991 }, 
{ 197.5, 0.966093766, 162.7421168, 0.039774136 }, 
{ 198.5, 0.971038162, 162.7718741, 0.03976685 }, 
{ 199.5, 0.975938391, 162.8002371, 0.03976007 }, 
{ 200.5, 0.980785418, 162.8272889, 0.039753741 }, 
{ 201.5, 0.985571579, 162.8531067, 0.039747815 }, 
{ 202.5, 0.99029042, 162.8777619, 0.039742249 }, 
{ 203.5, 0.994936555, 162.9013208, 0.039737004 }, 
{ 204.5, 0.999505539, 162.9238449, 0.039732048 }, 
{ 205.5, 1.003993753, 162.9453912, 0.039727352 }, 
{ 206.5, 1.0083983, 162.9660131, 0.03972289 }, 
{ 207.5, 1.012716921, 162.9857599, 0.03971864 }, 
{ 208.5, 1.016947912, 163.0046776, 0.039714581 }, 
{ 209.5, 1.021090055, 163.0228094, 0.039710697 }, 
{ 210.5, 1.025142554, 163.0401953, 0.039706971 }, 
{ 211.5, 1.029104983, 163.0568727, 0.039703391 }, 
{ 212.5, 1.032977233, 163.0728768, 0.039699945 }, 
{ 213.5, 1.036759475, 163.0882404, 0.039696623 }, 
{ 214.5, 1.040452117, 163.1029943, 0.039693415 }, 
{ 215.5, 1.044055774, 163.1171673, 0.039690313 }, 
{ 216.5, 1.047571238, 163.1307866, 0.039687311 }, 
{ 217.5, 1.050999451, 163.1438776, 0.039684402 }, 
{ 218.5, 1.054341482, 163.1564644, 0.039681581 }, 
{ 219.5, 1.057598512, 163.1685697, 0.039678842 }, 
{ 220.5, 1.060771808, 163.1802146, 0.039676182 }, 
{ 221.5, 1.063862715, 163.1914194, 0.039673596 }, 
{ 222.5, 1.066872639, 163.202203, 0.039671082 }, 
{ 223.5, 1.069803036, 163.2125835, 0.039668635 }, 
{ 224.5, 1.072655401, 163.2225779, 0.039666254 }, 
{ 225.5, 1.075431258, 163.2322024, 0.039663936 }, 
{ 226.5, 1.078132156, 163.2414722, 0.039661679 }, 
{ 227.5, 1.080759655, 163.2504019, 0.039659481 }, 
{ 228.5, 1.083315329, 163.2590052, 0.039657339 }, 
{ 229.5, 1.085800751, 163.2672954, 0.039655252 }, 
{ 230.5, 1.088217496, 163.2752848, 0.039653218 }, 
{ 231.5, 1.090567133, 163.2829854, 0.039651237 }, 
{ 232.5, 1.092851222, 163.2904086, 0.039649306 }, 
{ 233.5, 1.095071313, 163.297565, 0.039647424 }, 
{ 234.5, 1.097228939, 163.304465, 0.039645591 }, 
{ 235.5, 1.099325619, 163.3111185, 0.039643804 }, 
{ 236.5, 1.101362852, 163.3175349, 0.039642063 }, 
{ 237.5, 1.103342119, 163.3237231, 0.039640367 }, 
{ 238.5, 1.105264876, 163.3296918, 0.039638715 }, 
{ 239.5, 1.107132561, 163.3354491, 0.039637105 }, 
{ 240, 1.108046193, 163.338251, 0.039636316 }
	};
	return LMS;
}
