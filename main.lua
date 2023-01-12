-- define console variables (dont mess with these)
r_program = false -- resets the board when console has been entered
r_program_on = 1 -- must be set to 1, makes the program work

-- define program variables below. it is recommended to make all of these local.

local quaternion = {}

do
  local sqrt = math.sqrt

  local quatMt = {}

  function quatMt.__index(tab,key)
    return quaternion[key]
  end
  
  local function new(r,i,j,k)
    r = r or 0
    i = i or 0
    j = j or 0
    k = k or 0
    local a = {r,i,j,k}
    setmetatable(a,quatMt)
    return a
  end

  local function newAll(r,i,j,k)
    local a = {r,i,j,k}
    setmetatable(a,quatMt)
    return a
  end
  
  local function add(q1,q2)
    return newAll(q1[1]+q2[1],q1[2]+q2[2],q1[3]+q2[3],q1[4]+q2[4])
  end

  quatMt.__add = add
  
  local function sub(q1,q2)
    return newAll(q1[1]-q2[1],q1[2]-q2[2],q1[3]-q2[3],q1[4]-q2[4])
  end

  quatMt.__sub = sub

  local function mul(q1,q2)
    local r1,i1,j1,k1 = q1[1],q1[2],q1[3],q1[4]
    local r2,i2,j2,k2 = q2[1],q2[2],q2[3],q2[4]
    return newAll(
      r1*r2 - i1*i2 - j1*j2 - k1*k2,
      r1*i2 + i1*r2 + j1*k2 - k1*j2,
      r1*j2 + j1*r2 + k1*i2 - i1*k2,
      r1*k2 + k1*r2 + i1*j2 - j1*i2
    )
  end

  quatMt.__mul = mul

  local function norm(q)
    local r,i,j,k = q[1],q[2],q[3],q[4]
    return sqrt(r*r+i*i+j*j+k*k)
  end

  local function inv(q)
    local r,i,j,k = q[1],q[2],q[3],q[4]
    local u = 1/(r*r+i*i+j*j+k*k)
    return newAll(r*u,-i*u,-j*u,-k*u)
  end

  local function sign(val)
    if val > 0 then return ' + '
    elseif val < 0 then return ' - '
    else return end
  end

  local tstr = {'','i','j','k'}
  local abs = math.abs

  local function tostring(q)
    local s = {sign(q[1]),sign(q[2]),sign(q[3]),sign(q[4])}
    local first = true
    local str = ""
    for i=1,4 do
      if s[i] then
        if not first then
          str = str .. s[i] .. abs(q[i]) .. tstr[i]
        else
          str = q[i] .. tstr[i]
        end
        first = false
      end
    end
    return str
  end

  quaternion.new = new
  quaternion.add = add
  quaternion.sub = sub
  quaternion.mul = mul
  quaternion.inv = inv
  quaternion.tostring = tostring
end

do
  local width,height = 71,45
  local width2,height2 = width/2,height/2

  local CUTOFF_MODE = 1
  --[[
    0: No cutoff
      Not a great idea, because you will see things that should be behind you
    1: Basic cutoff
      Any line involving points with a z value less than the cutoff value will be destroyed.
    2: Advanced cutoff
      Any line involving points with a z value less than the cutoff value will be destroyed if both points are less than the cutoff. Otherwise, it will draw between the point with positive z and the point on the original line that crosses the cutoff value. Not implemented
  ]]
  local CUTOFF_VALUE = 0.01

  --[[
  2 layer object definition
  
  {
  Points,
  Polygons
  }

  Points contain 3d coordinates (x,y,z)
  Polygons contain numbers that reference 3 or more points.
  
  Polygons also contain another point that acts as a normal vector. If one is not present, it will be calculated based on the first three points.
  
  The normal vector is used to determine which polygons to draw.
  ]]--

  local object = {}

  local vector = {}

  do
    local sqrt = math.sqrt
  
    local function new(x,y,z)
      return {x,y,z}
    end

    local function add(v1,v2)
      return new(v1[1]+v2[1],v1[2]+v2[2],v1[3]+v2[3])
    end

    local function sub(v1,v2)
      return new(v1[1]-v2[1],v1[2]-v2[2],v1[3]-v2[3])
    end

    local function reverse(v1)
      return new(-v1[1],-v1[2],-v1[3])
    end

    local function scale(v,s)
      return new(v[1]*s,v[2]*s,v[3]*s)
    end

    local function norm(v)
      local x,y,z = v[1],v[2],v[3]
      return sqrt(x*x+y*y+z*z)
    end

    local function normalize(v)
      return scale(v,1/norm(v))
    end

    local function dotproduct(v1,v2)
      return v1[1]*v2[1] + v1[2]*v2[2] + v1[3]*v2[3]
    end

    local function crossproduct(v1,v2)
      return new(
        v1[2]*v2[3] - v1[3]*v2[2],
        v1[3]*v2[1] - v1[1]*v2[3],
        v1[1]*v2[2] - v1[2]*v2[1]
      )
    end
  
    vector.new = new
    vector.add = add
    vector.reverse = reverse
    vector.scale = scale
    vector.norm = norm
    vector.normalize = normalize
    vector.dotproduct = dotproduct
    vector.crossproduct = crossproduct
  end

  local CAMERA = {pos = {x=0,y=0,z=0},rot = quaternion.new(1,0,0,0)}

  local function drawLine(x1,y1,x2,y2)
    
  end

  local function projectPoint(point)
    local px = point[1]
    local py = point[2]
    local pz = point[3]
    pz = pz/px * width2 + width2
    py = py/px * width2 + height2
    return {pz,py}
  end

  do
    local newVector = vector.new
    local VectorCrossProd = vector.crossproduct
    local VectorDotProd = vector.dotproduct
    local invertVector = vector.reverse
    local normalizeVector = vector.normalize
    local sqrt = math.sqrt

    local FRUST_X = 1
    local FRUST_Y = height/width
  
    local function new(pointList,polygons)     
      local obj = {}
      obj.points = pointList
      local pol = {}
      for i = 1,#polygons do
        local p = polygons[i]
        if #p < 3 then
          error("Polygon #".. i .." is not a polygon")
        end
        if p.norm then
          pol[i] = p
        else
          -- Calculate normal vector from first 2 lines
          local point
          local v1
          do
            local p1 = pointList[p[1]]
            local p2 = pointList[p[2]]
            v1 = newVector(p2[1]-p1[1],p2[2]-p1[2],p2[3]-p1[3])
            point = p1
          end
          local v2
          do
            local p1 = pointList[p[2]]
            local p2 = pointList[p[3]]
            v2 = newVector(p2[1]-p1[1],p2[2]-p1[2],p2[3]-p1[3])
          end
          local normal = normalizeVector(VectorCrossProd(v1,v2))
          -- Make sure it points out from center
          local opNorm = invertVector(normal)
          local d1 = {point[1]+normal[1],point[2]+normal[2],point[3]+normal[3]}
          local d2 = {point[1]+opNorm[1],point[2]+opNorm[2],point[3]+opNorm[3]}
          d1 = sqrt(d1[1]*d1[1] + d1[2]*d1[2] + d1[3]*d1[3])
          d2 = sqrt(d2[1]*d2[1] + d2[2]*d2[2] + d2[3]*d2[3])
          if d2 > d1 then
            p.norm = opNorm
          else
            p.norm = normal
          end
          pol[i] = p
        end
      end
      obj.polygons = pol
      obj.rotation = quaternion.new(1,0,0,0)
      obj.pos = {0,0,0}
      obj.scale = 1
      obj.important = false
      -- If important, then skip riskier culling steps
      local size = 0
      for i=1,#pointList do
        local p = pointList[i]
        local s = p[1]*p[1] + p[2]*p[2] + p[3]*p[3]
        if s > size then size = s end
      end
      obj.size = sqrt(size)
      return obj
    end

    function object.setCameraPos(x,y,z)
      CAMERA.pos = {x=x,y=y,z=z}
    end

    function object.setCameraRot(quat)
      CAMERA.rot = quaternion.new(quat[1],quat[2],quat[3],quat[4])
    end

    function object.setLineFunction(func)
      drawLine = func
    end

    local drawFuncList = {
      [0] = function(points,polygons)
        local projected = {}
        for i = 1,#points do
          local p = points[i]
          projected[i] = projectPoint(p)
        end
        for i = 1,#polygons do
          local p = polygons[i]
          for j=1,(#p-1) do
            drawLine(projected[p[j]],projected[p[j+1]])
          end
          drawLine(projected[p[#p]],projected[p[1]])
        end
      end,
    [1] = function(points,polygons) -- CUTOFF 1
      local projected = {}
      local valid = {}
      for i = 1,#points do
        local p = points[i]
        projected[i] = projectPoint(p)
        valid[i] = (p[1] >= CUTOFF_VALUE)
      end
      for i = 1,#polygons do
        local p = polygons[i]
        for j=1,(#p-1) do
          if valid[p[j]] and valid[p[j+1]] then
            drawLine(projected[p[j]],projected[p[j+1]])
          end
        end
        if valid[p[#p]] and valid[p[1]] then
          drawLine(projected[p[#p]],projected[p[1]])
        end
      end
    end,
      [2] = function(points,polygons)
  
      end,
    }

    local drawObject = drawFuncList[CUTOFF_MODE]
  
  function object.render(obj)
    local NPointList = {}
    local NormList = {}
    -- Align all points and normal vectors
    do
      local OldList = obj.points
      local rotation = (CAMERA.rot:inv())
      local cp = CAMERA.pos
      local cpx = cp.x
      local cpy = cp.y
      local cpz = cp.z
      local newPos = quaternion.new(0,obj.pos[1]-cpx,obj.pos[2]-cpy,obj.pos[3]-cpz)
      newPos = rotation * newPos * rotation:inv()
      local npx = newPos[2]
      local npy = newPos[3]
      local npz = newPos[4]
      local sc = obj.scale
      do -- culling gang
        if npx < CUTOFF_VALUE then -- trivial cull
          return -1
        end
        if not obj.important then
          local s = obj.size * obj.scale
          local prx,pry = npz/npx,npy/npx
          local K = npx-(s*0.707)
          if K < CUTOFF_VALUE then
            K = CUTOFF_VALUE
          end
          local ps = s/K
          local a = (prx+ps > -1) and (prx-ps < 1)
          if not a then return 0 end
          local b = (pry+ps > -FRUST_Y) and (pry-ps < FRUST_Y)
          if not b then return 0 end
        end
      end
      rotation = rotation * obj.rotation
      local rotinv = rotation:inv()
      for i=1,#obj.points do
        local point = OldList[i]
        -- Rotate first
        local quat = quaternion.new(0,point[1],point[2],point[3])
        quat = rotation * quat * rotinv
        -- Translate and push to new list
        NPointList[i] = {quat[2]+npx,quat[3]+npy,quat[4]+npz}
      end
      for i=1,#obj.polygons do
        local point = obj.polygons[i].norm
        -- Rotate first
        local quat = quaternion.new(0,point[1],point[2],point[3])
        quat = rotation * quat * rotinv
        -- Push to normal list
        NormList[i] = {quat[2],quat[3],quat[4]}
      end
    end
    -- Determine which polygons to render
    local polyList = {}
    for i=1,#obj.polygons do
      local poly = obj.polygons[i]
      local v0 = NPointList[poly[1]]
      v0 = {-v0[1],-v0[2],-v0[3]}
      local norm = NormList[i]
      if VectorDotProd(v0,norm) >= 0 then -- Backface culling
        polyList[#polyList+1] = poly
      end
    end
    -- Actually render the polygons
    drawObject(NPointList,polyList)
  end

    object.new = new
  
  end
  _G["obj3d"] = object
  _G["vector"] = vector
end

--[[ END LIBRARIES ]]--

obj3d.setLineFunction(function(p1,p2)
  hc_draw_line(p1[1],p1[2],p2[1],p2[2],511)
end)

local object = obj3d.new(
  { -- Points
    [1] = {0,0,-1},
    [2] = {-0.8,0,0},
    [3] = {-0.8,0,0.3},
    [4] = {0.8,0,0},
    [5] = {0.8,0,0.3},
    [6] = {0,0.25,-0.4},
    [7] = {0,-0.25,-0.4},
    [8] = {0,-0.25,0.35},
    [9] = {0,0.25,0.35},
  },
  { -- Polygons
    [1] = {1,2,6},
    [2] = {1,4,6},
    [3] = {1,2,7},
    [4] = {1,4,7},
    [5] = {2,6,9},
    [6] = {2,3,9},
    [7] = {6,9,4},
    [8] = {9,5,4},
    [9] = {3,2,8},
    [10] = {4,5,8},
    [11] = {2,7,8},
    [12] = {7,8,4},
    [13] = {5,9,8},
    [14] = {9,8,3},
  }
)

object.pos = {10,0,0}

local campos = {0,0,0}
local camrot = quaternion.new(1,0,0,0)

local function newRotation(theta,x,y,z)
  local val = math.sqrt(x*x+y*y+z*z)
  local s = math.sin(theta/2)
  local c = math.cos(theta/2)
  return quaternion.new(c,s*x/val,s*y/val,s*z/val)
end

local rotator
do
  local apFrame = 0.005
  local A = {2,3,1.2}
  local value = math.sqrt(A[1]*A[1] + A[2]*A[2] + A[3]*A[3])
  A = {A[1]/value,A[2]/value,A[3]/value}
  local s = math.sin(apFrame/2)
  local c = math.cos(apFrame/2)
  rotator = quaternion.new(c,A[1]*s,A[2]*s,A[3]*s)
end
local rinv = rotator:inv()

local rawIsDown = player.keypressed

local function isDown(value)
  return tolua(rawIsDown(keys[value]))
end

local VECTORF = quaternion.new(0,1,0,0)
local VECTORL = quaternion.new(0,0,0,-1)

local RSPEED = 0.02
local MSPEED = 0.04

function program_refresh() -- must exist globally, this is what the console runs.
  hc_draw_square(0,0,70,44,0)

  --[[ ROTATION CONTROL ]]--

  if isDown("W") then
    -- rotate up
    camrot = camrot * newRotation(RSPEED,0,0,1)
  elseif isDown("S") then
    -- rotate down
    camrot = camrot * newRotation(-RSPEED,0,0,1)
  end
  if isDown("A") then
    -- rotate left
    camrot = camrot * newRotation(RSPEED,0,1,0)
  elseif isDown("D") then
    -- rotate right
    camrot = camrot * newRotation(-RSPEED,0,1,0)
  end
  if isDown("Q") then
    -- spin left
    camrot = camrot * newRotation(RSPEED,1,0,0)
  elseif isDown("E") then
    -- spin right
    camrot = camrot * newRotation(-RSPEED,1,0,0)
  end

  --[[ MOVEMENT CONTROL ]]--

  if isDown("UP") then
    local vf = camrot * VECTORF * camrot:inv()
    campos[1] = campos[1] + MSPEED*vf[2]
    campos[2] = campos[2] + MSPEED*vf[3]
    campos[3] = campos[3] + MSPEED*vf[4]
  elseif isDown("DOWN") then
    local vf = camrot * VECTORF * camrot:inv()
    campos[1] = campos[1] - MSPEED*vf[2]
    campos[2] = campos[2] - MSPEED*vf[3]
    campos[3] = campos[3] - MSPEED*vf[4] 
  end

  if isDown("LEFT") then
    local vl = camrot * VECTORL * camrot:inv()
    campos[1] = campos[1] + MSPEED*vl[2]
    campos[2] = campos[2] + MSPEED*vl[3]
    campos[3] = campos[3] + MSPEED*vl[4]
  elseif isDown("RIGHT") then
    local vl = camrot * VECTORL * camrot:inv()
    campos[1] = campos[1] - MSPEED*vl[2]
    campos[2] = campos[2] - MSPEED*vl[3]
    campos[3] = campos[3] - MSPEED*vl[4] 
  end

  --[[ RENDERING ]]--

  obj3d.setCameraPos(campos[1],campos[2],campos[3])
  obj3d.setCameraRot(camrot)
  object.rotation = rotator * object.rotation
  obj3d.render(object)
end

player.alert("God I hope this works")
