## code to prepare `DATASET` dataset goes here

# Number wards
nwards <- 5
# Ward names
ward_names <- letters[1:nwards]
# Number of patients per day
pop_size_P <- rep(20,nwards)
# Number of HCWS per day
pop_size_H <- rep(15,nwards)
# Number of visitors per day
nVisits <- rep(10,nwards)
# Length of stay
LS <- rep(27,nwards)


toydata <- list(nwards = nwards,
               ward_names = ward_names,
               pop_size_P = pop_size_P,
               pop_size_H = pop_size_H,
               nVisits = nVisits,
               LS = LS)

usethis::use_data(toydata, overwrite = TRUE)
