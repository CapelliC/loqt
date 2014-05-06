/*
    lqXDot_test   : interfacing Qt and Graphviz library

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
    Copyright (C) : 2013,2014 Carlo Capelli

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

#include <QApplication>
#include "mainwindow.h"
#include "lqXDot.h"
#include "lqXDot_configure.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    //a.setWindowIcon(QIcon(":images/lqXDot_test.png"));

    {   using namespace configure_behaviour;
        option_enable(move_Edges);
    }

    // xdot required
    setlocale(LC_ALL, "C");
    lqXDot::registerMetaTypes();

    MainWindow w(argc, argv);
    w.show();

    return a.exec();
}
