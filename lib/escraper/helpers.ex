defmodule Escraper.Helpers do

  def hash_url(url) do
    :crypto.sha(url)
  end

  def url_off_root?(url, root_url) do
  end

  def parse_links_from_html(html) do
    pattern = %r/<a href=["|'](.*?)["|']>/
    Enum.map Regex.scan(pattern, html), &Enum.at(&1,1)
  end

  def data_is_html?(data) do
    !data_is_image?(data)
  end

  def data_is_image?(data) do
    data_is_jpeg?(data) or data_is_png?(data) or data_is_gif?(data)
  end

  def data_is_jpeg?(data) do
    size(data) >= 10 and 
      (binary_part(data, 6, 4) == "JFIF" or binary_part(data, 6, 4) == "Exif")
  end

  def data_is_png?(data) do
    size(data) >= 8 and binary_part(data, 0, 8) == "\211PNG\r\n\032\n"
  end

  def data_is_gif?(data) do
    size(data) >= 6 and
      (binary_part(data, 0, 6) == "GIF87a" or binary_part(data, 0, 6) == "GIF89a")
  end

  def print_tree(html) do
    tree = :mochiweb_html.parse(html)
    print_branch(tree, 0)
  end

  def print_branch({ tag, attrs, children }, depth // 0) do
    padded = String.rjust(tag, String.length(tag) + depth)
    IO.puts "#{padded} :: #{inspect(attrs))}"
    Enum.each(children, &print_branch(&1, depth+1))
  end


end
