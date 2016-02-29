/*
    lq3D         : interfacing Qt3D

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2016

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

#include "lq3dScene.h"

namespace lq3d_configure {

    enum configure_options {
        render_by_clpfd
    };

    inline int bit(configure_options o)  { return 1 << o; }

    //inline bool option_is_on(configure_options N) { return (lq3dScene::configure_behaviour & bit(N)) == bit(N); }
    //inline void option_enable(configure_options N) { lq3dScene::configure_behaviour |= bit(N); }

    enum element_association_keys {
        Edges_Items
    };

}

#endif // CONFIGURE_BEHAVIOUR_H