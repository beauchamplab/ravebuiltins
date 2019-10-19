# if(FALSE) {
#     require(rave)
# subject = Subject$new(project_name = 'congruency', subject_code = 'YAB')
# 
# brain = rave_brain2(subject)
# table = load_meta('electrodes', subject_id = subject$id)[, c('Electrode','Coord_x','Coord_y','Coord_z','Label')]
# 
# 
# trans = read.table(file.path(subject$dirs$suma_dir, 'T1_to_freesurf.txt'), head = F)
# trans = as.matrix(trans)
# brain$scanner_center
# 
# # because the center of the (AFNI) T1 is relative the origin of the scanner, we need to translate back to get into
# # FREESURFER RAS space
# new_coord = trans %*% rbind(table$Coord_x, table$Coord_y, table$Coord_z, 1) + c(brain$scanner_center, 0)
# 
# 
# # store the values back into the electrodes meta table
# table$Coord_x = new_coord[1,]
# table$Coord_y = new_coord[2,]
# table$Coord_z = new_coord[3,]
# 
# 
# # Set back to brain and calculate nearest nodes
# table$SurfaceElectrode = TRUE
# brain$set_electrodes(table)
# 
# brain$plot()
# 
# 
# new_elec = brain$calculate_template_coordinates(save_to = FALSE)
# new_elec$Radius[59:66] = 0.5
# 
# # save_meta('electrodes', project_name = subject$project_name, subject_code = subject$subject_code, data = new_elec)
# 
# 
# 
# aseg <- threeBrain::read_fs_mgh_mgz('/Volumes/data/UT/YAB/iELVis_localization/YAB/mri/brainmask.mgz')
# dat = aseg$get_data()
# fields::image.plot(dat[,,128])
# fields::image.plot(dat[,,128]==0)
# unique(as.vector(dat))
# 
# 
# anat <- read.table('/Volumes/data/UT/YAB/iELVis_localization/YAB/label/aparc.annot.a2009s.ctab', header=F)
# head(anat)
# tran_elec= new_elec[,2:4]
# 
# 
# junk = t(as.matrix(anat[,3:5]))
# junk = (brain$Torig %*% rbind(junk, 1))[1:3,]
# 
# mapping = t(apply(tran_elec, 1, function(te) {
#     dist = colSums((junk - te)^2)
#     ii = which.min(dist)
#     c(ii, sqrt(dist[ii]))
# }))
# 
# l = anat$V2[mapping[,1]]
# 
# cbind(new_elec$Label, as.character(l))
# 
# new_elec$Anat = l
# 
# brain$set_electrode_values(new_elec)
# 
# brain$plot()
# }
