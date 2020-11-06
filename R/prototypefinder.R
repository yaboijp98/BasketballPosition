#' Pickup Basketball Prototype Finder
#' @param relativeHeight from 1-5 how do you rank in height with your teammates
#' @param shooter logical, are you a shooter
#' @param ballhandler logical, can you dribble well
#' @param defense logical, are you a good defender
#' @return player prototype
#' @export
#' @example
#' findPrototype(relativeHeight=2,shooter=TRUE,ballhandler=FALSE,defense=TRUE)


findPrototype = function(relativeHeight, shooter, ballhandler, defense) {
  #find general position
  if (relativeHeight == 1) {
    gen_pos = "back court"
  }
  else if (relativeHeight == (2 || 3)) {
    gen_pos = "wing"
  }
  else {
    gen_pos = "front court"
  }

  #offensive role
  if (gen_pos == "back court") {
    if (shooter == T && ballhandler == T) {
      role = "Point God"
    }
    else if (shooter == T && ballhandler == F) {
      role = "Sharpshooter"
    }
    else if (shooter == F && ballhandler == T) {
      role = "Play Facilitator"
    }
    else {
      role = "Left Bench"
    }
  }
  if (gen_pos == "wing") {
    if (shooter == T && ballhandler == T) {
      role = "Walking Bucket"
    }
    else if (shooter == T && ballhandler == F) {
      role = "Sharpshooter"
    }
    else if (shooter == F && ballhandler == F) {
      role = "Playmaking Wing"
    }
    else {
      role = "Hustle Guy"
    }
  }
  if (gen_pos == "front court") {
    if (shooter == T && ballhandler == T) {
      role = "Eastern European"
    }
    else if (shooter == T && ballhandler == F) {
      role = "Floor Spacer"
    }
    else if (shooter == F && ballhandler == T) {
      role = "Poor Man's Nikola Jokic"
    }
    else {
      role = "Big Body"
    }
  }

  #Defense
  if (defense == T) {
    if (role == "Point God") {
      role = "John Stockon's Son"
    }
    else if (role == "Sharpshooter") {
      role = "3 & D"
    }
    else if (role == "Play Facilitator") {
      role = "Two Way Guard"
    }
    else if (role == "Left Bench") {
      role = "Just Go Home"
    }
    else if (role == "Walking Bucket") {
      role = "All-Around Superstar"
    }
    else if (role == "Playmaking Wing") {
      role = "Glue Guy"
    }
    else if (role == "Hustle Guy") {
      role = "Wing Defender"
    }
    else if (role == "Eastern European") {
      role = "Superstar Big Man"
    }
    else if (role == "Floor Spacer") {
      role = "Star Big Man"
    }
    else if (role == "Poor Man's Nikola Jokic") {
      role = "Athletic P&R Specialist"
    }
    else {
      role = "Rim Protector"
    }
  }

  return(role)
}
