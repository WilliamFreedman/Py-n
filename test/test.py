# Declaration w/ assignment
x : int = 5

# Arithmetic w/ assignment
result : float = 3.14 * (2 + 3)

# Logical operators
flag : bool = true && false || !true
print(flag)

# While loop w/ counter
counter: int = 0
while counter < 10:
	counter += 1

print(counter)


# If-else conditional

if x > 5:
	y : int = 10
else :
	y : int = 20
  
print(y)


# Nested control w/ break
if x <= 100:
	while x != 0 :
		x -= 1
		if x == 10 :
			break
print(x)

# Type casting w/ control stmt
x : int = 1
y : float = x + 3.14

if y > 4:
	while x < 5 :
		x += 1
else:
	print(x)

# Class definition
  class Circle:
	  radius : int

	  def area() -> float:
		  return 3.14159 * (this.radius * this.radius)
