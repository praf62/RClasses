1+1
2*3
6/2
exp(3)
log(10)
1:3
a = 51:100
indexes = c(1,3,4)
indexes <<- c(1,3,4)#para ponteiros
c(2,3,4) -> indexes
a[a>80]
a[1:5]
b = 1:50
c = a+b
d = a*b
tb = cbind(a, b, c, d)
View(tb)
tb[2,3]
tb[a>80,3]

bool = c(TRUE, FALSE, TRUE, FALSE, FALSE)
bool[bool==TRUE]
which(bool==TRUE)
?which

e = c(1.2, 3.2, 3.6, 4.1)
mean(e)

f = c("feminino", "masculino", "feminino", "masculino")
class(f)
class(e)
class(d)
g = c("muito estressada", "pouco estresada", "muito estressada")
class(g) = "ordered"
