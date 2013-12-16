/*
    lqUty        : loqt utilities

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

#ifndef MRUHELPER_H
#define MRUHELPER_H

#include "lqUty_global.h"

#include <QMenu>
#include <QSettings>
#include <QFileInfo>
#include <QSignalMapper>

/** factorize boring code required to handle MRU
  */
class LQUTYSHARED_EXPORT MruHelper
{
public:

    /** name from path */
    static inline QString path2title(QString path) {
        return QFileInfo(path).fileName();
    }

    /** keep QSettings keys with reasonable defaults */
    MruHelper(QString key = "mru", QString entry = "path");

    /** load the menu and initialize actions */
    void loadMru(QSettings &s, QObject *parent, const char* slot = SLOT(openFileIndex(int)));

    /** write menu entries to QSettings storage */
    void storeMru(QSettings &s);

    /** store the path, if not yet available, or make it first */
    bool insertPath(QObject* parent, QString path);

    /** strip path from menu, if available, and update it */
    bool removePath(QObject *parent, QString path);

protected:

    /** keep entries at runtime */
    QMenu* mruMenu;

    /** section and elements keys */
    QString key, entry;

    /** assign files to menu entries */
    QSignalMapper *cmdMapper;

    /** can't recover path from mapper */
    QStringList files;

    /** pass the index in files */
    QString slot;

    /** mapping files' entries to actions */
    void fillMru(QObject* parent);

    /** limit max number of files */
    int max_files;
};

#endif // MRUHELPER_H
