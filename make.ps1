ls src | foreach { erlc -o ebin $_.fullname }
ls test | foreach {erlc -o ebin $_.fullname }