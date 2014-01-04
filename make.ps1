ls src | foreach { erlc +debug_info -o ebin $_.fullname }
ls test | foreach {erlc +debug_info -o ebin $_.fullname }