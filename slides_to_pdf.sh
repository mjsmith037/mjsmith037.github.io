function slides_to_pdf {
    sudo docker run --rm -t --net=host -v `pwd`:/slides astefanutti/decktape $1 $2
}
