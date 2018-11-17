-- Rhythms:
rhythms = [
  "[0 0 0]/2"
  ,"[~ 0] ~ 0"
  ,"[0 0 ~]"
  ,"[0 ~]"
  ,"[[~ fs fs]*2"
  ,"[0*8]/12"
  ,"[~/3 0]/2"
  ]

mvel offset = velocity ((abs(sine) * 0.25) + offset)

cr rhy vel p_offset = (n rhy |-| n p_offset) # mvel vel 

a = cr (rhythms!!0) 0.4 -24
a2 = n ("[~ 2] ~ 2" - 24) # 0.3 ~> mvel
b = n ("[c c ~]"-24) # 0.4 ~> mvel
b2 = n ("[c ~]"-24) # 0.4 ~> mvel 
c = n ("[[~ fs fs] [~ fs fs], [g*8]/12, [~/3 as]/2]"-24) # 0.5 ~> mvel

m1 $ (stack [a2, b2, c]) # mvel

