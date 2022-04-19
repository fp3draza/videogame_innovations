#!/bin/bash
user=$1

to_be_linked="/mnt/data_science_project/downloads"
local_dir="/home/$user/videogame_downloads"

#create a link from the user's home to the download folder
if sudo [ -d "$local_dir" ]; then
  echo $'\n the folder \n' $local_dir $'\n exists and will be removed \n'
  sudo rm -rf $local_dir
  else 
  echo  $'\n the folder \n' $local_dir $'\n does not exist and will be created \n' 
fi
sudo ln -s $to_be_linked $local_dir

# change the owner from root to the current user
sudo chown -R $user:$user $local_dir