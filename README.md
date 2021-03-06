### TwitterApi

Вам необходимо реализовать api для создания твиттов, получения твитта и лайка твитта

#### Создание твитта
На вход к вам поступает CreateTweetRequest, из которого вы должны создать объект Tweet, обязательно проверив длину
текста (а может быть потом появятся и другие проверки).

hashTags вычисляется из tweet.text, собирая все слова, начинающиеся с символа `#`.

tweet.id генерируется из `UUID.randomUUID.toString`.

При условии прохождения всех проверок твит сохраняется в базу данных (вы можете реализовать любой способ сохранения:
в памяти, запись в файл, или то, что вам захочется).
После выполнения всех операций должен вернуться созданный объект.

#### Получение твитта
На вход к вам поступает GetTweetRequest, вернуть вы должны объект tweet, если он найдем по id.

#### Лайк твитта
Должен обновлять количество лайков у твитта и возвращать новое значение.
Если твит не найдет, то должна возвращаться ошибка

Все функции должны возвращать значение типа Result[T]
в котором может лежать либо текст ошибки, либо результат выполнение

Заготовка находится в [TweetApp.scala](src/main/scala/fintech/homework05/TweetApp.scala)