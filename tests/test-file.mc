int main()
{
  File f;
  Word space;
  int s;
  space = calloc(1,500);
  f = open("test-file.txt", "r");
  s = read(space, 1, 500, f);
  printword(space);
  close(f);
  free(space);
  return 0;
}
