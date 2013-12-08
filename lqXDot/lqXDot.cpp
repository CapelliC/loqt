/*
    lqXDot       : interfacing Qt and Graphviz library

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013, Carlo Capelli

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "lqXDot.h"
#include "lqXDotView.h"
#include <QDebug>

lqXDot::lqXDot()
{
}

int lqXDot::mt_lqXDotView;

int lqXDot::mt_GVC_t;
int lqXDot::mt_Agraph_t;
int lqXDot::mt_Agnode_t;
int lqXDot::mt_Agedge_t;
int lqXDot::mt_Agsym_t;
int lqXDot::mt_Agrec_t;

void lqXDot::registerMetaTypes()
{
    //! make available thru reflection
    mt_lqXDotView = qRegisterMetaType<lqXDotView>("lqView");

    mt_GVC_t      = qRegisterMetaType<GVC_t*    >("GVC_t*");
    mt_Agraph_t   = qRegisterMetaType<Agraph_t* >("Agraph_t*");
    mt_Agnode_t   = qRegisterMetaType<Agnode_t* >("Agnode_t*");
    mt_Agedge_t   = qRegisterMetaType<Agedge_t* >("Agedge_t*");
    mt_Agsym_t    = qRegisterMetaType<Agsym_t*  >("Agsym_t*");
    mt_Agrec_t    = qRegisterMetaType<Agrec_t*  >("Agrec_t*");
/*
    qDebug() <<
        mt_lqView <<
        mt_GVC_t << mt_Agraph_t << mt_Agnode_t <<
        mt_Agedge_t << mt_Agsym_t << mt_Agrec_t;
*/
}
