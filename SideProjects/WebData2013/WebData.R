### script to package data for website

vuln <- read.csv('SideProjects/BenH_Jan27_2015/vulnerability_weighting_matrix.csv')
write.csv(vuln, '/var/cache/halpern-et-al/var/www/ebm-site/GlobalMarine2013/SupplementalData/vulnerability_matrix.csv', row.names=FALSE)
file.remove('/var/cache/halpern-et-al/var/www/ebm-site/GlobalMarine2013/index2.html')
file.remove('/var/cache/halpern-et-al/var/www/ebm-site/GlobalMarine2013/index.html')
