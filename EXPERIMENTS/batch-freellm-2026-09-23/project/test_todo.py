
import unittest
from todo import Todo

class TestTodo(unittest.TestCase):
    def test_add_task(self):
        todo = Todo()
        todo.add_task('Task 1')
        self.assertIn('Task 1', todo.tasks)

    def test_remove_task(self):
        todo = Todo()
        todo.add_task('Task 1')
        todo.remove_task('Task 1')
        self.assertNotIn('Task 1', todo.tasks)

    def test_list_tasks(self):
        todo = Todo()
        todo.add_task('Task 1')
        todo.add_task('Task 2')
        self.assertEqual(todo.list_tasks(), ['Task 1', 'Task 2'])

if __name__ == '__main__':
    unittest.main()
