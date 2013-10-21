defmodule Escraper.Helpers do

  def hash_url(url) do
    :crypto.sha(url)
  end

end
