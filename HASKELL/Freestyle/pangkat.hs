pang (a,n) = if n == 0
    then 1
    else a * pang(a,n-1)
main = print (pang(5,6))    