rm debugscheduler*
cabal clean;cabal configure;cabal build;./dist/build/poet/poet data/test.src
for file in $(ls *.dot)
do
  pngfile=$(echo ${file} | sed -e "s/.dot/.png/") 
  dot -Tpng ${file} -o${pngfile}
done
eog debugscheduler00.png &
