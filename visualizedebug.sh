for file in $(ls *.dot)
do
  pngfile=$(echo ${file} | sed -e "s/.dot/.png/") 
  dot -Tpng ${file} -o${pngfile}
done
eog debugscheduler00.png &
