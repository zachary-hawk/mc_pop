#!/usr/bin/env python 
import numpy as np
from scipy.io import FortranFile
import plotly.offline as py
import sys

try:
    bins=int(sys.argv[1])
except:
    bins=5

f = FortranFile("demographics.pop",'r', '>u4')
n=f.read_record([('len','i')])
n=n['len'][0]
data_scale=202
year=[]
age=[]
people=[]
gender=[]
max_dat=0


# This bit puts all theinformation into the right form
for i in range(n*data_scale):
    
    check_1=f.read_record([("year",'i4'),("age",'i4'),("people",'i4'),("gender",'i')])

    year.append((check_1[0])[0])
    age.append((check_1[0])[1])
    people.append((check_1[0])[2])
    gender.append((check_1[0])[3])
    


figure = {
    'data': [],
    'layout': {},
    'frames': [],
}

figure['layout']['yaxis'] = {'title': 'Age'}
figure['layout']['hovermode'] = 'closest'
figure['layout']['barmode'] ='overlay'
figure['layout']['bargap'] = 0.1
figure['layout']['title'] = "Population Pyramid" 


sliders_dict = {
    'active': 0,
    'yanchor': 'top',
    'xanchor': 'left',
    'currentvalue': {
        'font': {'size': 20},
        'prefix': 'Year: ',
        'visible': True,
        'xanchor': 'right'
    },
    'transition': {'duration': 300, 'easing': 'cubic-in-out'},
    'pad': {'b': 10, 't': 50},
    'len': 0.8,
    'x': 0.1,
    'y': 0.,
    'steps': []
}    

for m in range(1,n+1):
    new_year=year[(m-1)*data_scale:m*data_scale][0]
    frame = {'data': [], 'name': str(new_year), 'traces':[m]}

    new_age=age[(m-1)*data_scale:m*data_scale]
    new_people=people[(m-1)*data_scale:m*data_scale]
    new_gender=gender[(m-1)*data_scale:m*data_scale]
    total=np.sum(new_people)/100
    new_people=new_people/total
    
    men_age=[]
    women_age=[]
    men=[]
    women=[]
    frame = {"data": [], "name": str(new_year)}
        
    for i in range(0,len(new_age)):
        if new_gender[i]==0:
            # Man
            men_age.append(new_age[i])
            men.append(new_people[i])
        else:
            # Woman
            women_age.append(new_age[i])
            women.append(new_people[i])
            
            
            
            
    men=np.array(men)
    women=-np.array(women)
    
    men_aug=[]
    women_aug=[]
    age_aug=np.array(list(range(0,101,bins)))
    buff_men=0
    buff_women=0
    '''
    bins_lower=np.zeros(bins)
    bins_upper=np.zeros(bins)
    bins_lower[-1]=0
    bins_upper[-1]=100
    for i in range(0,bins):
        bins_lower[i]=i*100/bins
        bins_upper[i]=i+1)
    '''
    for i in range(0,101,bins):
        for j in range(0,100):
            if men_age[j]>=i and men_age[j]<i+bins:
                #print("range: ",i,i+bins)
                #print("age: ",men_age[j])
                buff_men+=men[j]
                buff_women+=women[j]
        men_aug.append(buff_men)
        women_aug.append(buff_women)
        buff_men=0
        buff_women=0
    '''for j in range(0,100):
        if men_age[j]==0:
            men_aug[0]+=men[j]
            women_aug[0]+=women[j]'''

    men_aug=np.array(men_aug)
    women_aug=np.array(women_aug)

    

    if np.max(np.concatenate((men_aug,-women_aug)))>max_dat:        
        max_dat=np.max(np.concatenate((men_aug,-women_aug)))



    if m==1:
        vis=True
    else :
        vis =False
        
    data_men = {'type':"bar",
                      "y":age_aug,
                      'x':men_aug,
                      'orientation':'h',
                      'name':'Men',
                      'hoverinfo':'x',
                      'marker':dict(color='dodgerblue'),
                'visible':vis
                
    }
    data_women = {'type': "bar",
                  'y':age_aug,
                       'x':women_aug,
                       'orientation':'h',
                       'name':'Women',
                       'text':-1 * np.array(women_aug).astype('int'),
                       'hoverinfo':'x',
                       'marker':dict(color='plum'),
                  'visible':vis
    
    }

    figure['data'].append(data_men)
    figure['data'].append(data_women)
    
    frame['data'].append(data_men)
    frame['data'].append(data_women)
    
    figure['frames'].append(frame)

    '''slider_step = {'args': [
        [new_year],
        {'visible' :   [False] * ((2*n+1)+1),
        'frame': {'duration': 300, 'redraw': True},
         'mode': 'immediate',
         'transition': {'duration': 300}}
    ],
                   'label': str(new_year),
                   'method': 'animate'} '''

    slider_step = dict(
	method="restyle",
	args=["visible",[False] * ((2*n+1)+1)],
        #'frame', dict(duration= 300, redraw= True),
        #'mode','immediate', 
        #'transition',{'duration': 300}
        #],

        label= str(new_year)
    )
    
    slider_step["args"][1][2*(m-1)] = True
    slider_step["args"][1][2*(m-1)+1] = True

    sliders_dict['steps'].append(slider_step)

figure['layout']['sliders'] = [sliders_dict]
    #figure['layout']['showlegend'] = True
'''
figure['layout']['updatemenus'] = [
    {
        'buttons': [
            {
                'args': [None, {'frame': {'duration': 500, 'redraw': True},
                         'fromcurrent': True, 'transition': {'duration': 300, 'easing': 'quadratic-in-out'}}],
                'label': 'Play',
                'method': 'animate'
            },
            {
                'args': [[None], {'frame': {'duration': 0, 'redraw': True}, 'mode': 'immediate',
                'transition': {'duration': 0}}],
                'label': 'Pause',
                'method': 'animate'
            }
        ],
        'direction': 'left',
        'pad': {'r': 10, 't': 87},
        'showactive': True,
        'type': 'buttons',
        'x': 0.1,
        'xanchor': 'right',
        'y': 0,
        'yanchor': 'top'
    }
]
'''


figure['layout']['xaxis'] = { 'title': 'Population %','range':[-1.1*max_dat,1.1*max_dat],'showgrid':True} #,"tickvals":[-15,-10,-5,0,5,10,15],"ticktext":[15,10,5,0,5,10,15]}

#print(sliders_dict)
    
#fig=go.Figure(figure,{'config':{'scrollzoom': True}})
#fig.show()
py.plot(figure,auto_play=False,filename="pop_pyramid.html",animation_opts={'fromcurrent':True})
        
