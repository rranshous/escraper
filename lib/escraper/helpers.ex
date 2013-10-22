defmodule Escraper.Helpers do

  def hash_url(url) do
    :crypto.sha(url)
  end

  def url_off_root?(url, root_url) do
    # TODO: make better
    String.slice(url, 0, String.length(root_url)) == root_url
  end

  def parse_links_from_html(html) do
    tree = tree_from_html(html)
    Enum.uniq(Enum.filter(parse_links_from_tree(tree), fn(l) -> l != nil end))
  end

  def tree_from_html(html) do
    try do
      :mochiweb_html.parse(html)
    rescue
      _ -> IO.puts "FAILed parse"
    end
  end

  def parse_links_from_tree({ "a", args, children }) do
    [ args_get(args, "href") | parse_links_from_children(children) ]
  end

  def parse_links_from_tree({ tag, args, children }) do
    parse_links_from_children(children)
  end

  def parse_links_from_tree(content) do
    []
  end

  def parse_links_from_children(children) do
    Enum.flat_map children, &parse_links_from_tree(&1)
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
    IO.puts "#{padded} :: #{inspect(attrs)}"
    Enum.each(children, &print_branch(&1, depth+1))
  end

  def args_get(args, arg) do
    case Enum.find(args, fn({k,v}) -> arg ==  k end) do
      { ^arg, value } -> value
      _ -> nil
    end
  end

end
