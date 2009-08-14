/*

model (snapshot) = Life
  state grid[101,101] = 0 // how to specify initial value?

  iterator x = 1..100
  iterator y = 1..100
  iterator xn = -1..1
  iterator yn = -1..1

  grid[0,_] = 0
  grid[last,_] = 0
  grid[_,0] = 0
  grid[_,last] = 0

  gridSum[x,y] = sum(grid[x+xn,y+yn]) - grid[x,y]

  grid[x,y] = {1 when grid[x,y] == 1 and gridSum[x,y] == 4 or gridSum[x,y] == 3,
               0 otherwise}
  

  output snapshot = grid[_,_]
end

model (snapshot) = Life
  iterator x = 1..100
  iterator y = 1..100
  state grid[x,y] = 0

//  equation grid[0,_] = 0
//  equation grid[last,_] = 0
//  grid[_,0] = 0
//  grid[_,last] = 0

equation  gridSum[x,y] = grid[x-1,y-1] + grid[x-1,y] + grid[x-1,y+1] + grid[x,y-1] + grid[x,y+1] +  grid[x+1,y-1] + grid[x+1,y] + grid[x+1,y+1]

equation  grid[x,y] = {1 when grid[x,y] == 1 and gridSum[x,y] == 4 or gridSum[x,y] == 3,
          	       0 otherwise}
  

  output snapshot = grid[_,_]
end



model (value) = Cell (grid, x, y)
  iterator xn = -1..1
  iterator yn = -1..1

  neighborsum = sum(grid[x+xn,y+yn]) - grid[x,y]
  
  value = {1 when grid[x,y] == 1 and gridSum[x,y] == 4 or gridSum[x,y] == 3,
           0 otherwise}
  
end

model (snapshot) = Life
  iterator x=1..100
  iterator y=1..100
  submodel Cell grid[x,y] with {grid=grid, x=x, y=y}

  output snapshot = grid.value
end




model (value) = Cell (grid[x,y])
  iterator xn = -1..1
  iterator yn = -1..1

  neighborsum = sum(grid[x+xn,y+yn]) - grid[x,y]
  
  value = {1 when grid[x,y] == 1 and gridSum[x,y] == 4 or gridSum[x,y] == 3,
           0 otherwise}
  
end

model (snapshot) = Life
  iterator x=1..100
  iterator y=1..100
  submodel Cell grid[x,y] with {grid=grid[_,_]}
  
  output snapshot = grid.value
end
*/

/*
model (snapshot) = life
  iterator x = 1..100
  iterator y = 1..100
  state grid[x,y] = 0

  equation  gridSum[x,y] = grid[n, x-1,y-1] + grid[n, x-1,y] + grid[n, x-1,y+1] + grid[n, x,y-1] + grid[n, x,y+1] +  grid[n, x+1,y-1] + grid[n, x+1,y] + grid[n, x+1,y+1]

  equation  grid[n+1,x,y] = {1 when grid[n, x,y] == 1 and gridSum[x,y] == 4 or gridSum[x,y] == 3,
            	         0 otherwise}
  

  output snapshot = grid[n, _,_]
end
*/

model (snapshot) = life
  iterator x = 1..100
  iterator y = 1..100
  state grid[x,y] = 0
  
  equation lookup (x,y) => {grid[n, x, y] when x > 0 and x <= 100 and y > 0 and y <= 100,
  	   	  	    0 otherwise}

  equation  gridSum[x,y] = lookup(x-1,y-1) + lookup(x-1,y) + lookup(x-1,y+1) + lookup(x,y-1) + lookup(x,y+1) +  lookup(x+1,y-1) + lookup(x+1,y) + lookup(x+1,y+1)

  equation  grid[n+1,x,y] = {1 when grid[n, x,y] == 1 and gridSum[x,y] == 4 or gridSum[x,y] == 3,
            	         0 otherwise}
  

  output snapshot = grid/*implicitly: [n, _,_]*/
end