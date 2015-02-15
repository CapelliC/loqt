/*
    lqXDot       : interfacing Qt and Graphviz library

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015

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

#ifndef LQXDOT_CONFIGURE_H
#define LQXDOT_CONFIGURE_H

#include "lqXDotScene.h"

namespace configure_behaviour {

    enum configure_options {
        move_Edges,
        associate_Edges_items,
        no_draw_Graph_bounding_box,
        no_box_on_render_errors
    };

    inline int bit(configure_options o)  { return 1 << o; }

    inline bool option_is_on(configure_options N) { return (lqXDotScene::configure_behaviour & bit(N)) == bit(N); }
    inline void option_enable(configure_options N) { lqXDotScene::configure_behaviour |= bit(N); }

    enum element_association_keys {
        Edges_Items
    };

}

#endif // CONFIGURE_BEHAVIOUR_H
