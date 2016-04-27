/*
    pqConsole    : interfacing SWI-Prolog and Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016

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

#ifndef PQPROOFSCENE_H
#define PQPROOFSCENE_H

#include <QGraphicsScene>
#include <QGraphicsItem>
#include "pqConsole_global.h"

/**
 * @brief The pqProofScene class
 *  display and incremental building of proof tree
 */
class PQCONSOLESHARED_EXPORT pqProofScene : public QGraphicsScene
{
    Q_OBJECT
public:
    pqProofScene();
    ~pqProofScene();

    class node : public QGraphicsItem {

    };
    class link : public QGraphicsItem  {

    };

    //! construction: add trace text
    void addNode(QString line);

    //! layout compute
    void layout();
};

#endif // PQPROOFSCENE_H
